use oxsea_ir::{IRGraph, IRNodeId, IR_END_ID, IR_START_ID};

pub fn topological_sort(graph: &IRGraph) -> std::vec::Vec<IRNodeId> {
    let mut visited = vec![false; graph.len()];
    let mut result = std::vec::Vec::with_capacity(graph.len());

    fn dfs(
        graph: &IRGraph,
        node_id: IRNodeId,
        visited: &mut std::vec::Vec<bool>,
        result: &mut std::vec::Vec<IRNodeId>,
    ) {
        if visited[node_id] {
            return;
        }
        visited[node_id] = true;
        for output in graph.get_node(node_id).outputs() {
            dfs(graph, *output, visited, result);
        }
        result.push(node_id);
    }

    dfs(graph, IR_START_ID, &mut visited, &mut result);

    result.reverse();
    result
}

pub fn reverse_topological_sort(graph: &IRGraph) -> std::vec::Vec<IRNodeId> {
    let mut visited = vec![false; graph.len()];
    let mut result = std::vec::Vec::with_capacity(graph.len());

    fn dfs(
        graph: &IRGraph,
        node_id: IRNodeId,
        visited: &mut std::vec::Vec<bool>,
        result: &mut std::vec::Vec<IRNodeId>,
    ) {
        if visited[node_id] {
            return;
        }
        visited[node_id] = true;
        for input in graph.get_node(node_id).inputs() {
            dfs(graph, *input, visited, result);
        }
        result.push(node_id);
    }

    dfs(graph, IR_END_ID, &mut visited, &mut result);

    result
}
