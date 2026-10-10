use std::fs;
use std::path::Path;

use combinatoric_core::{Composition, Graph, Partition};
use num_rational::Ratio;
use polytool::{check_weak_interlacing, is_real_rooted};
use serde_json::{json, Value};
use sym_poly_core::UnivariatePolynomial;
use sym_poly_multipoly::{atom_polynomial, key_polynomial, schubert_polynomial, MultiPoly};
use sym_poly_qsym::QSymFunction;
use sym_poly_sym::{
    chromatic_symmetric, lah_symmetric_elementary, lah_symmetric_monomial, nabla_eigenvalue,
    petrie_symmetric, unicellular_llt,
    unicellular_llt_q_plus_one_e_expansion, Basis, SymmetricFunction,
};
use sym_poly_sym::kostka::{kostka_coefficient, sn_character};

type Rational = Ratio<i64>;

fn p(parts: &[u32]) -> Partition {
    Partition::new(parts.to_vec())
}

fn c(parts: &[u32]) -> Composition {
    Composition::new(parts.to_vec())
}

fn partition_value(partition: &Partition) -> Value {
    json!(partition.parts())
}

fn rational_value(value: &Rational) -> Value {
    if *value.denom() == 1 {
        json!(*value.numer())
    } else {
        json!(format!("{}/{}", value.numer(), value.denom()))
    }
}

fn terms_i64(function: &SymmetricFunction<i64>) -> Value {
    Value::Array(
        function
            .terms()
            .iter()
            .map(|(shape, coefficient)| json!([shape.parts(), coefficient]))
            .collect(),
    )
}

fn terms_q(function: &SymmetricFunction<UnivariatePolynomial<i64>>) -> Value {
    Value::Array(
        function
            .terms()
            .iter()
            .map(|(shape, coefficient)| json!([shape.parts(), coefficient.coeffs()]))
            .collect(),
    )
}

fn terms_qsym(function: &QSymFunction<i64>) -> Value {
    Value::Array(
        function
            .terms()
            .iter()
            .map(|(composition, coefficient)| json!([composition.parts(), coefficient]))
            .collect(),
    )
}

fn terms_multipoly(function: &MultiPoly<i64>) -> Value {
    Value::Array(
        function
            .terms()
            .iter()
            .map(|(exponents, coefficient)| json!([exponents, coefficient]))
            .collect(),
    )
}

fn all_compositions(total: u32) -> Vec<Vec<u32>> {
    fn rec(remaining: u32, current: &mut Vec<u32>, output: &mut Vec<Vec<u32>>) {
        if remaining == 0 {
            output.push(current.clone());
            return;
        }
        for first in 1..=remaining {
            current.push(first);
            rec(remaining - first, current, output);
            current.pop();
        }
    }
    let mut output = Vec::new();
    if total == 0 {
        output.push(Vec::new());
    } else {
        rec(total, &mut Vec::new(), &mut output);
    }
    output
}

fn all_area_sequences(n: usize) -> Vec<Vec<u8>> {
    if n == 0 {
        return vec![Vec::new()];
    }
    fn rec(index: usize, current: &mut [u8], output: &mut Vec<Vec<u8>>) {
        if index == current.len() {
            output.push(current.to_vec());
            return;
        }
        let upper = index.min(current[index - 1] as usize + 1);
        for value in 0..=upper {
            current[index] = value as u8;
            rec(index + 1, current, output);
        }
    }
    let mut output = Vec::new();
    rec(1, &mut vec![0; n], &mut output);
    output
}

fn write_json(directory: &Path, filename: &str, value: Value) {
    let path = directory.join(filename);
    let text = serde_json::to_string_pretty(&value).expect("JSON serialization failed");
    fs::write(path, format!("{text}\n")).expect("fixture write failed");
}

fn write_kostka(directory: &Path) {
    let mut records = Vec::new();
    for n in 0..=7 {
        let partitions = Partition::all_of_size(n);
        for lambda in &partitions {
            for mu in &partitions {
                records.push(json!({
                    "lambda": lambda.parts(),
                    "mu": mu.parts(),
                    "value": kostka_coefficient(lambda, mu)
                }));
            }
        }
    }
    write_json(
        directory,
        "kostka.json",
        json!({
            "family": "Kostka",
            "rust_function": "sym_poly_sym::kostka::kostka_coefficient",
            "convention": "K(lambda,mu) counts SSYT of shape lambda and content mu; partitions are Rust's reverse-lex order only for matrix data, while records are keyed by the displayed vectors.",
            "records": records
        }),
    );
}

fn write_lr(directory: &Path) {
    let inputs: &[(&[u32], &[u32])] = &[
        (&[], &[1]),
        (&[1], &[1]),
        (&[2], &[1]),
        (&[2, 1], &[1]),
        (&[3], &[2, 1]),
        (&[2, 1], &[2, 1]),
        (&[3, 1], &[2, 1]),
        (&[3, 2], &[2, 1]),
        (&[2, 1], &[1, 1, 1]),
        (&[2, 2], &[2, 1]),
    ];
    let mut records = Vec::new();
    for (lambda_parts, mu_parts) in inputs {
        let lambda = p(lambda_parts);
        let mu = p(mu_parts);
        let outputs = Partition::all_of_size(lambda.size() + mu.size());
        let product = SymmetricFunction::<i64>::schur_symmetric(lambda.clone())
            .multiply(&SymmetricFunction::<i64>::schur_symmetric(mu.clone()))
            .to_schur_basis();
        for nu in &outputs {
            records.push(json!({
                "lambda": lambda.parts(),
                "mu": mu.parts(),
                "nu": nu.parts(),
                "value": product.coefficient(nu)
            }));
        }
    }
    write_json(
        directory,
        "lr.json",
        json!({
            "family": "Littlewood-Richardson",
            "rust_function": "SymmetricFunction::multiply followed by to_schur_basis",
            "convention": "s_lambda*s_mu = Sum_nu c(lambda,mu,nu) s_nu; empty partitions are included and total degree is at most 8.",
            "records": records
        }),
    );
}

fn write_characters(directory: &Path) {
    let mut records = Vec::new();
    for n in 1..=7 {
        let partitions = Partition::all_of_size(n);
        for lambda in &partitions {
            for mu in &partitions {
                records.push(json!({
                    "lambda": lambda.parts(),
                    "mu": mu.parts(),
                    "value": sn_character(lambda, mu)
                }));
            }
        }
    }
    write_json(
        directory,
        "characters.json",
        json!({
            "family": "Symmetric-group characters",
            "rust_function": "sym_poly_sym::kostka::sn_character",
            "convention": "value is chi^lambda(mu), with lambda indexing the irreducible and mu the cycle type; n is at most 7.",
            "records": records
        }),
    );
}

fn write_transitions(directory: &Path) {
    let bases = [
        ("m", Basis::Monomial),
        ("e", Basis::Elementary),
        ("h", Basis::CompleteH),
        ("p", Basis::PowerSum),
        ("s", Basis::Schur),
    ];
    let mut records = Vec::new();
    for degree in 1..=6 {
        let partitions = Partition::all_of_size(degree);
        let mut matrices = serde_json::Map::new();
        for (source_name, source_basis) in bases {
            for (target_name, target_basis) in bases {
                let matrix: Vec<Value> = partitions
                    .iter()
                    .map(|source_partition| {
                        let source = SymmetricFunction::<Rational>::basis_element(
                            source_basis,
                            source_partition.clone(),
                        );
                        let target = source.to_basis(target_basis);
                        Value::Array(
                            partitions
                                .iter()
                                .map(|target_partition| {
                                    rational_value(&target.coefficient(target_partition))
                                })
                                .collect(),
                        )
                    })
                    .collect();
                matrices.insert(format!("{source_name}->{target_name}"), Value::Array(matrix));
            }
        }
        records.push(json!({
            "degree": degree,
            "partitions": partitions.iter().map(partition_value).collect::<Vec<_>>(),
            "matrices": matrices
        }));
    }
    write_json(
        directory,
        "transitions.json",
        json!({
            "family": "classical symmetric-function transitions",
            "rust_function": "SymmetricFunction::to_basis",
            "convention": "rows are source basis elements and columns are target basis elements, both indexed by the listed reverse-lex partitions; coefficients are integers or p/q strings.",
            "basis_order": ["m", "e", "h", "p", "s"],
            "records": records
        }),
    );
}

fn write_macdonald(directory: &Path) {
    let mut records = Vec::new();
    for n in 1..=4 {
        for partition in Partition::all_of_size(n) {
            let b_terms: Vec<Value> = partition
                .diagram_boxes()
                .into_iter()
                .map(|(row, col)| json!([col, row, 1]))
                .collect();
            let nabla = nabla_eigenvalue(&partition);
            let nabla_terms: Vec<Value> = nabla
                .coeffs()
                .iter()
                .enumerate()
                .flat_map(|(t_degree, q_polynomial)| {
                    q_polynomial
                        .coeffs()
                        .iter()
                        .enumerate()
                        .filter(|(_, coefficient)| **coefficient != Ratio::from_integer(0))
                        .map(move |(q_degree, coefficient)| {
                            json!([q_degree, t_degree, coefficient.numer(), coefficient.denom()])
                        })
                })
                .collect();
            records.push(json!({
                "partition": partition.parts(),
                "B_terms": b_terms,
                "nabla_terms": nabla_terms,
                "nabla_convention": "[q_degree,t_degree,numerator/denominator]"
            }));
        }
    }
    write_json(
        directory,
        "macdonald-operators.json",
        json!({
            "family": "modified Macdonald operator eigenvalues",
            "rust_function": "sym_poly_sym::macdonald::{macdonald_b_eigenvalue,nabla_eigenvalue}",
            "convention": "B_terms list q^a' t^l' over English diagram boxes; nabla is q^(n(lambda')) t^(n(lambda)). Coefficients are [q-degree,t-degree,numerator,denominator].",
            "records": records
        }),
    );
}

fn write_llt(directory: &Path) {
    let mut records = Vec::new();
    for n in 1..=4 {
        for area in all_area_sequences(n) {
            let raw = unicellular_llt(&area);
            let shifted_e = unicellular_llt_q_plus_one_e_expansion(&area)
                .expect("generated area sequence must be valid");
            let schur = raw.to_schur_basis();
            records.push(json!({
                "area": area,
                "edges_zero_based": sym_poly_sym::unit_interval_edges(&area),
                "monomial_q_terms": terms_q(&raw),
                "schur_q_terms": terms_q(&schur),
                "q_plus_one_elementary_terms": terms_q(&shifted_e)
            }));
        }
    }
    write_json(
        directory,
        "llt.json",
        json!({
            "family": "unicellular LLT",
            "rust_function": "sym_poly_sym::{unicellular_llt,unicellular_llt_q_plus_one_e_expansion}",
            "convention": "area is a zero-based unit-interval area sequence with area[0]=0 and area[i]<=i; edges are listed zero-based. Polynomial coefficient vectors are ascending q-degree. The q_plus_one field substitutes q -> q+1 and then converts to e.",
            "records": records
        }),
    );
}

fn write_chromatic(directory: &Path) {
    let mut records = Vec::new();
    for n in 1..=4 {
        for area in all_area_sequences(n) {
            let edges = sym_poly_sym::unit_interval_edges(&area);
            let graph = Graph::new(area.len(), &edges);
            let function = chromatic_symmetric::<i64>(&graph);
            records.push(json!({
                "area": area,
                "edges_zero_based": edges,
                "monomial_terms": terms_i64(&function),
                "schur_terms": terms_i64(&function.to_schur_basis())
            }));
        }
    }
    write_json(
        directory,
        "chromatic.json",
        json!({
            "family": "chromatic symmetric functions",
            "rust_function": "sym_poly_sym::chromatic_symmetric",
            "convention": "X_G is in the monomial basis with the usual coloring normalization; these records use the unit-interval graph of the displayed area sequence and q=1.",
            "records": records
        }),
    );
}

fn write_qsym(directory: &Path) {
    let mut records = Vec::new();
    for total in 1..=4 {
        for alpha in all_compositions(total) {
            let function = QSymFunction::<i64>::fundamental_qsym(c(&alpha));
            let monomial = function.to_monomial_basis();
            records.push(json!({
                "alpha": alpha,
                "fundamental_terms": terms_qsym(&function),
                "monomial_terms": terms_qsym(&monomial)
            }));
        }
    }
    let left = QSymFunction::<i64>::fundamental_qsym(c(&[1, 2]));
    let right = QSymFunction::<i64>::fundamental_qsym(c(&[1]));
    let product = left.multiply(&right);
    write_json(
        directory,
        "quasisymmetric.json",
        json!({
            "family": "quasisymmetric fundamental/monomial",
            "rust_function": "QSymFunction::fundamental_qsym, QSymFunction::to_monomial_basis, QSymFunction::multiply",
            "convention": "alpha is a composition in left-to-right order; F_alpha = sum_{beta refines alpha} M_beta. Product records use the quasi-shuffle product.",
            "records": records,
            "product": {
                "left": [1, 2],
                "right": [1],
                "monomial_terms": terms_qsym(&product.to_monomial_basis()),
                "fundamental_terms": terms_qsym(&product.to_fundamental_basis())
            }
        }),
    );
}

fn write_nonsymmetric(directory: &Path) {
    let compositions: Vec<Vec<u32>> = vec![vec![0, 2], vec![1, 2], vec![2, 1], vec![1, 0, 2], vec![0, 1, 2]];
    let mut key_atom = Vec::new();
    for alpha in &compositions {
        let key = key_polynomial::<i64>(&alpha);
        let atom = atom_polynomial::<i64>(&alpha);
        key_atom.push(json!({
            "alpha": alpha,
            "key_terms": terms_multipoly(&key),
            "atom_terms": terms_multipoly(&atom)
        }));
    }
    let permutations = [[1, 2, 3], [2, 1, 3], [1, 3, 2], [2, 3, 1], [3, 1, 2], [3, 2, 1]];
    let schubert = permutations
        .into_iter()
        .map(|permutation| {
            json!({
                "permutation": permutation,
                "terms": terms_multipoly(&schubert_polynomial::<i64>(&permutation))
            })
        })
        .collect::<Vec<_>>();
    write_json(
        directory,
        "nonsymmetric.json",
        json!({
            "family": "key, atom, and Schubert polynomials",
            "rust_function": "sym_poly_multipoly::{key_polynomial,atom_polynomial,schubert_polynomial}",
            "convention": "exponent vectors are in x_1,...,x_n order; key(alpha) uses the standard weak-composition key convention and Schubert permutations are one-line, one-indexed.",
            "key_atom": key_atom,
            "schubert": schubert
        }),
    );
}

fn write_eulerian(directory: &Path) {
    let polynomials = polytool::sequences::eulerian_polynomials_bigint(6)
        .into_iter()
        .map(|coefficients| {
            coefficients
                .into_iter()
                .map(|coefficient| coefficient.to_string().parse::<i64>().unwrap())
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();
    let root_cases = vec![
        json!({"coefficients": [1, 11, 11, 1], "real_rooted": is_real_rooted(&[1, 11, 11, 1])}),
        json!({"coefficients": [1, 0, 1], "real_rooted": is_real_rooted(&[1, 0, 1])}),
        json!({"coefficients": [1, 2, 1], "real_rooted": is_real_rooted(&[1, 2, 1])}),
    ];
    let interlacing_cases = vec![
        json!({"left": [1, 4, 1], "right": [1, 11, 11, 1], "value": check_weak_interlacing(&[1, 4, 1], &[1, 11, 11, 1])}),
        json!({"left": [1, 1], "right": [-1, 1], "value": check_weak_interlacing(&[1, 1], &[-1, 1])}),
        json!({"left": [-1, 1], "right": [1, 1], "value": check_weak_interlacing(&[-1, 1], &[1, 1])}),
    ];
    write_json(
        directory,
        "eulerian.json",
        json!({
            "family": "Eulerian and real-rootedness checks",
            "rust_function": "polytool::{sequences::eulerian_polynomials_bigint,is_real_rooted,check_weak_interlacing}",
            "convention": "Eulerian coefficient vectors are ascending powers of t; the sequence is A_1 through A_6. Interlacing is directed and follows check_weak_interlacing(left,right).",
            "eulerian": polynomials,
            "root_cases": root_cases,
            "interlacing_cases": interlacing_cases
        }),
    );
}

fn write_lah_petrie(directory: &Path) {
    let mut lah = Vec::new();
    for n in 1..=4 {
        for k in 1..=n {
            lah.push(json!({
                "n": n,
                "k": k,
                "elementary_terms": terms_i64(&lah_symmetric_elementary(n, k)),
                "monomial_terms": terms_i64(&lah_symmetric_monomial(n, k))
            }));
        }
    }
    let petrie = [(2, 1), (2, 4), (3, 4), (4, 5)]
        .into_iter()
        .map(|(k, n)| {
            let value = petrie_symmetric::<i64>(k, n);
            json!({"k": k, "n": n, "monomial_terms": terms_i64(&value), "schur_terms": terms_i64(&value.to_schur_basis())})
        })
        .collect::<Vec<_>>();
    write_json(
        directory,
        "lah-petrie.json",
        json!({
            "family": "Lah and Petrie symmetric functions",
            "rust_function": "sym_poly_sym::{lah_symmetric_elementary,lah_symmetric_monomial,petrie_symmetric}",
            "convention": "Lah records use the Rust L_(n,k) convention and list both e and m bases. Petrie G(k,n) is the sum of m_lambda over lambda_1<k.",
            "lah": lah,
            "petrie": petrie
        }),
    );
}

fn main() {
    let directory = Path::new(env!("CARGO_MANIFEST_DIR"));
    write_kostka(directory);
    write_lr(directory);
    write_characters(directory);
    write_transitions(directory);
    write_macdonald(directory);
    write_llt(directory);
    write_chromatic(directory);
    write_qsym(directory);
    write_nonsymmetric(directory);
    write_eulerian(directory);
    write_lah_petrie(directory);
    println!("wrote Rust cross-check fixtures to {}", directory.display());
}
