use itertools::Itertools;
use std::collections::HashSet;
use utils::read_line_by_line;

#[derive(Debug)]
struct Graph<'a> {
    nodes: HashSet<&'a str>,
    edges: HashSet<(&'a str, &'a str)>,
}

fn parse_input(input: &Vec<String>) -> Graph {
    let mut graph: Graph = Graph {
        nodes: Default::default(),
        edges: Default::default(),
    };
    input.iter().for_each(|line| {
        let nodes = line.split_once('-').map(|(n1, n2)| (n1, n2)).unwrap();
        graph.nodes.insert(nodes.0);
        graph.nodes.insert(nodes.1);
        graph.edges.insert((nodes.0, nodes.1));
        graph.edges.insert((nodes.1, nodes.0));
    });
    graph
}

fn problem1(graph: &Graph) -> usize {
    let valid_nodes = graph
        .nodes
        .iter()
        .filter(|n| n.chars().nth(0).unwrap() == 't')
        .collect::<Vec<_>>();
    let mut triple_connected_components: HashSet<(&str, &str, &str)> = HashSet::new();
    for node in valid_nodes {
        let mut neighbours: HashSet<&str> = HashSet::new();
        for edge in graph.edges.iter().filter(|(n, _)| n.contains(node)) {
            neighbours.insert(edge.1);
        }
        for couple in neighbours.iter().combinations(2) {
            // Check existence of a third edge
            if graph.edges.contains(&(couple[0], couple[1])) {
                let mut connected_component = vec![couple[0], couple[1], node];
                connected_component.sort();
                triple_connected_components.insert((
                    connected_component[0],
                    connected_component[1],
                    connected_component[2],
                ));
            }
        }
    }
    triple_connected_components.len()
}

fn problem2(graph: &Graph) -> String {
    fn bron_kerbosch<'a>(
        graph: &Graph<'a>,
        r: &HashSet<&'a str>,
        p: &mut HashSet<&'a str>,
        x: &mut HashSet<&'a str>,
        cliques: &mut Vec<HashSet<&'a str>>,
    ) {
        // Maximal clique
        if p.is_empty() && x.is_empty() {
            cliques.push(r.clone());
            return;
        }

        let mut p_iter: Vec<&str> = p.clone().into_iter().collect();
        while let Some(vertex) = p_iter.iter().next() {
            let mut neighbours: HashSet<&str> = HashSet::new();
            for edge in graph.edges.iter().filter(|(n, _)| n.contains(*vertex)) {
                neighbours.insert(edge.1);
            }

            let r_new = r.union(&HashSet::from([*vertex])).cloned().collect();
            let mut p_new = p.intersection(&neighbours).cloned().collect();
            let mut x_new = x.intersection(&neighbours).cloned().collect();

            bron_kerbosch(graph, &r_new, &mut p_new, &mut x_new, cliques);

            x.insert(vertex);
            p.remove(vertex);
            p_iter = p.clone().into_iter().collect();
        }
    }

    let mut cliques = Vec::new();
    bron_kerbosch(
        graph,
        &HashSet::new(),
        &mut graph.nodes.clone(),
        &mut HashSet::new(),
        &mut cliques,
    );
    let maximal_clique = cliques.iter().max_by_key(|x| x.len()).unwrap();
    let mut password: Vec<_> = maximal_clique.into_iter().collect();
    password.sort();
    password.iter().join(",")
}

fn main() {
    let input = read_line_by_line("../../day_23/input/example.txt", '\n');
    let graph = parse_input(&input);

    let part1 = problem1(&graph);
    println!("{}", part1);

    let part2 = problem2(&graph);
    println!("{:?}", part2);
}
