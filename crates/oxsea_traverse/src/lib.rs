use oxc_ast::ast::BinaryOperator;
use oxsea_ir::{
    CompileTimeValue, IRGraph, IRInstruction, IRNode, IRNodeId, IteratorKind, IR_END_ID,
    IR_INVALID_ID, IR_START_ID,
};
use sort::topological_sort;

#[macro_use]
mod macros;

mod sort;
pub mod transform;

pub struct Context<'i> {
    graph: &'i IRGraph,

    order: std::vec::Vec<IRNodeId>,
    ids: std::vec::Vec<IRNodeId>,
}

impl Context<'_> {
    pub fn new(graph: &IRGraph) -> Context {
        let order = topological_sort(graph);
        Context {
            graph,
            order,
            ids: vec![],
        }
    }

    pub fn new_with_ids(graph: &IRGraph) -> Context {
        let mut ids = vec![IR_INVALID_ID; graph.len()];
        ids[IR_END_ID] = IR_END_ID;
        ids[IR_START_ID] = IR_START_ID;

        Context {
            graph,
            order: vec![IR_INVALID_ID; graph.len()],
            ids,
        }
    }

    fn graph(&self) -> &IRGraph {
        self.graph
    }

    fn topological_order(&self) -> &std::vec::Vec<IRNodeId> {
        &self.order
    }

    fn node_id(&self, original_id: usize) -> usize {
        if self.ids.len() > 0 {
            self.ids[original_id]
        } else {
            original_id
        }
    }
}

pub trait IRNodeKind<'i> {
    fn ctx(&self) -> &'i Context<'i>;
    fn node(&self) -> &'i IRNode;
    fn node_id(&self) -> IRNodeId;
}

pub struct End<'i> {
    ctx: &'i Context<'i>,
    node: &'i IRNode,
    node_id: IRNodeId,
}
node_kind! { End }

pub struct Start<'i> {
    ctx: &'i Context<'i>,
    node: &'i IRNode,
    node_id: IRNodeId,
}
node_kind! { Start }

pub struct Unreachable<'i> {
    ctx: &'i Context<'i>,
    node: &'i IRNode,
    node_id: IRNodeId,
}
node_kind! { Unreachable }

pub struct Loop<'i> {
    ctx: &'i Context<'i>,
    node: &'i IRNode,
    node_id: IRNodeId,
}

impl Loop<'_> {
    input_id_accessor!(control_id, 0);
    input_id_accessor!(repeat_control_id, 1);
}
node_kind! { Loop }

pub struct Return<'i> {
    ctx: &'i Context<'i>,
    node: &'i IRNode,
    node_id: IRNodeId,
}

impl Return<'_> {
    input_id_accessor!(control_id, 0);
    input_id_accessor!(value_id, 1);
}
node_kind! { Return }

pub struct Add<'i> {
    ctx: &'i Context<'i>,
    node: &'i IRNode,
    node_id: IRNodeId,
}

impl Add<'_> {
    input_id_accessor!(left_id, 0);
    input_id_accessor!(right_id, 1);
}
node_kind! { Add }

pub struct Compare<'i> {
    ctx: &'i Context<'i>,
    node: &'i IRNode,
    node_id: IRNodeId,

    operator: &'i BinaryOperator,
}
node_kind! { Compare }

impl Compare<'_> {
    pub fn operator(&self) -> &BinaryOperator {
        self.operator
    }

    input_id_accessor!(left_id, 0);
    input_id_accessor!(right_id, 1);
}

pub struct Constant<'i> {
    ctx: &'i Context<'i>,
    node: &'i IRNode,
    node_id: IRNodeId,

    value: &'i CompileTimeValue,
}
node_kind! { Constant }

impl Constant<'_> {
    pub fn value(&self) -> &CompileTimeValue {
        self.value
    }
}

pub struct BindExport<'i> {
    ctx: &'i Context<'i>,
    node: &'i IRNode,
    node_id: IRNodeId,

    name: &'i str,
}
node_kind! { BindExport }

impl BindExport<'_> {
    input_id_accessor!(control_id, 0);
    input_id_accessor!(value_id, 1);

    pub fn name(&self) -> &str {
        self.name
    }
}

pub struct LoadGlobal<'i> {
    ctx: &'i Context<'i>,
    node: &'i IRNode,
    node_id: IRNodeId,

    name: &'i str,
}
node_kind! { LoadGlobal }

impl LoadGlobal<'_> {
    pub fn name(&self) -> &str {
        self.name
    }
}

pub struct IfElse<'i> {
    ctx: &'i Context<'i>,
    node: &'i IRNode,
    node_id: IRNodeId,

    resolved: &'i Option<bool>,
}
node_kind! { IfElse }

impl IfElse<'_> {
    input_id_accessor!(control_id, 0);
    input_id_accessor!(condition_id, 1);

    pub fn consequent_id(&self) -> IRNodeId {
        self.node.outputs()[0]
    }

    pub fn alternate_id(&self) -> IRNodeId {
        self.node.outputs()[1]
    }

    pub fn continuation_id(&self) -> Option<IRNodeId> {
        self.node.outputs().get(2).copied()
    }

    pub fn resolved(&self) -> &Option<bool> {
        self.resolved
    }
}

pub struct Proj<'i> {
    ctx: &'i Context<'i>,
    node: &'i IRNode,
    node_id: IRNodeId,

    index: usize,
}
impl Proj<'_> {
    input_id_accessor!(control_id, 0);
    input_id_accessor!(value_id, 0);

    pub fn index(&self) -> usize {
        self.index
    }
}
node_kind! { Proj }

pub struct Merge<'i> {
    ctx: &'i Context<'i>,
    node: &'i IRNode,
    node_id: IRNodeId,

    merge_input_index: usize,
    resolved_index: &'i Option<usize>,
}
node_kind! { Merge }

impl Merge<'_> {
    input_id_accessor!(branch_point_id, 0);

    pub fn phi_index(&self) -> Option<usize> {
        if self.merge_input_index == 0 {
            None
        } else {
            Some(self.merge_input_index - 1)
        }
    }

    pub fn has_phi_outputs(&self) -> bool {
        for output_id in self.node.outputs() {
            let phi_node = self.ctx.graph().get_node(*output_id);
            if *phi_node.instruction() == IRInstruction::Phi {
                return true;
            }
        }
        return false;
    }

    pub fn resolved_index(&self) -> &Option<usize> {
        self.resolved_index
    }

    pub fn merge_input_ids(&self) -> std::vec::Vec<IRNodeId> {
        return self.node.inputs()[1..]
            .iter()
            .map(|x| self.ctx.node_id(*x))
            .collect();
    }
}

pub struct Phi<'i> {
    ctx: &'i Context<'i>,
    node: &'i IRNode,
    node_id: IRNodeId,
}
node_kind! { Phi }

impl Phi<'_> {
    input_id_accessor!(control_id, 0);

    pub fn value_ids(&self) -> std::vec::Vec<IRNodeId> {
        return self.node.inputs()[1..]
            .iter()
            .map(|x| self.ctx.node_id(*x))
            .collect();
    }
}

pub struct PhiControl<'i> {
    ctx: &'i Context<'i>,
    node: &'i IRNode,
    node_id: IRNodeId,

    phi_input_index: usize,
}
node_kind! { PhiControl }

impl PhiControl<'_> {
    pub fn value_input_id(&self) -> IRNodeId {
        self.node.inputs()[self.phi_input_index + 1]
    }
}

pub struct PhiValue<'i> {
    ctx: &'i Context<'i>,
    node: &'i IRNode,
    node_id: IRNodeId,
}
node_kind! { PhiValue }

pub struct GetIterator<'i> {
    ctx: &'i Context<'i>,
    node: &'i IRNode,
    node_id: IRNodeId,
    kind: &'i IteratorKind,
}
impl GetIterator<'_> {
    input_id_accessor!(value_id, 0);

    pub fn kind(&self) -> &IteratorKind {
        &self.kind
    }
}
node_kind! { GetIterator }

pub struct IteratorNext<'i> {
    ctx: &'i Context<'i>,
    node: &'i IRNode,
    node_id: IRNodeId,
}
impl IteratorNext<'_> {
    input_id_accessor!(value_id, 0);
}
node_kind! { IteratorNext }

pub fn traverse<'i, T>(ir: &IRGraph, visit: &mut T)
where
    T: Visit,
{
    let ctx = Context::new(ir);
    visit.visit_start(&Start {
        ctx: &ctx,
        node: ir.get_node(IR_START_ID),
        node_id: IR_START_ID,
    });
}

pub trait Visit {
    fn enter_node(&mut self, _node_id: IRNodeId, _node: &IRNode) {}

    fn leave_node(&mut self, _node_id: IRNodeId, _node: &IRNode) {}

    fn visit_start(&mut self, start: &Start) {
        self.walk_start(start);
    }

    fn walk_start(&mut self, start: &Start) {
        enter_node!(self, start);

        self.visit_control_outputs(start);

        enter_node!(self, start);
    }

    fn visit_control_outputs<'i, ParentKind>(&mut self, parent: &ParentKind)
    where
        ParentKind: IRNodeKind<'i>,
    {
        let mut outputs = parent.node().outputs().clone();
        outputs.sort_by_key(|a| {
            parent
                .ctx()
                .topological_order()
                .iter()
                .position(|x| *x == *a)
        });

        for output in outputs {
            self.visit_control(parent, output);
        }
    }

    fn visit_control<'i, ParentKind>(&mut self, parent: &ParentKind, node_id: IRNodeId)
    where
        ParentKind: IRNodeKind<'i>,
    {
        let node = parent.ctx().graph().get_node(node_id);
        if node.is_dead_control() {
            return;
        }

        match node.instruction() {
            IRInstruction::Proj(index) => {
                self.visit_proj_control(&Proj {
                    ctx: parent.ctx(),
                    node: node,
                    node_id,

                    index: *index,
                });
            }
            IRInstruction::IfElse(resolved) => {
                self.visit_if_else(&IfElse {
                    ctx: parent.ctx(),
                    node: node,
                    node_id,

                    resolved,
                });
            }
            IRInstruction::Merge(resolved_index) => {
                let parent_id = parent.node_id();

                if let Some(value_idx) = resolved_index {
                    // Visit the node twice: Once to mark the resolution and once to continue the control flow.
                    self.visit_merge(&Merge {
                        ctx: parent.ctx(),
                        node: node,
                        node_id,
                        merge_input_index: 0,
                        resolved_index,
                    });
                    self.visit_merge(&Merge {
                        ctx: parent.ctx(),
                        node: node,
                        node_id,
                        merge_input_index: value_idx + 1,
                        resolved_index,
                    });
                } else {
                    let merge_input_index = node.inputs().iter().position(|x| *x == parent_id);
                    if let Some(merge_input_index) = merge_input_index {
                        self.visit_merge(&Merge {
                            ctx: parent.ctx(),
                            node: node,
                            node_id,
                            merge_input_index,
                            resolved_index,
                        });
                    }
                }
            }
            IRInstruction::BindExport(name) => {
                self.visit_bind_export(&BindExport {
                    ctx: parent.ctx(),
                    node: node,
                    node_id,
                    name: &name,
                });
            }
            IRInstruction::Return => {
                self.visit_return(&Return {
                    ctx: parent.ctx(),
                    node: node,
                    node_id,
                });
            }
            IRInstruction::End => {
                self.visit_end(&End {
                    ctx: parent.ctx(),
                    node: node,
                    node_id,
                });
            }
            IRInstruction::Loop => {
                let r#loop = Loop {
                    ctx: parent.ctx(),
                    node: node,
                    node_id,
                };
                match parent.node_id() {
                    id if id == r#loop.control_id() => {
                        self.visit_loop_head(&r#loop);
                    }
                    id if id == r#loop.repeat_control_id() => {
                        self.visit_loop_end(&r#loop);
                    }
                    _ => {
                        panic!(
                            "Unexpected input node {} for loop {:#?}",
                            parent.node_id(),
                            node
                        );
                    }
                }
            }
            i if !i.is_control() => {
                // Ignored, not a control.
            }
            _ => {
                panic!(
                    "Not implemented - visit_control: {:#?}",
                    parent.ctx().graph().get_node(node_id).instruction()
                );
            }
        }
    }

    fn visit_value<'i, ParentKind>(&mut self, parent: &ParentKind, node_id: IRNodeId)
    where
        ParentKind: IRNodeKind<'i>,
    {
        let node = parent.ctx().graph().get_node(node_id);
        match node.instruction() {
            IRInstruction::Constant(value) => {
                self.visit_constant(&Constant {
                    ctx: parent.ctx(),
                    node: node,
                    node_id: node_id,
                    value,
                });
            }
            IRInstruction::LoadGlobal(name) => {
                self.visit_load_global(&LoadGlobal {
                    ctx: parent.ctx(),
                    node: node,
                    node_id: node_id,
                    name: &name,
                });
            }
            IRInstruction::Add => {
                self.visit_add(&Add {
                    ctx: parent.ctx(),
                    node: node,
                    node_id: node_id,
                });
            }
            IRInstruction::Compare(operator) => {
                self.visit_compare(&Compare {
                    ctx: parent.ctx(),
                    node: node,
                    node_id: node_id,
                    operator: operator,
                });
            }
            IRInstruction::Phi => {
                self.visit_phi_value(&PhiValue {
                    ctx: parent.ctx(),
                    node: node,
                    node_id: node_id,
                });
            }
            IRInstruction::Proj(index) => {
                self.visit_proj_value(&Proj {
                    ctx: parent.ctx(),
                    node: node,
                    node_id: node_id,
                    index: *index,
                });
            }
            IRInstruction::IteratorNext => {
                self.visit_iterator_next(&IteratorNext {
                    ctx: parent.ctx(),
                    node: node,
                    node_id: node_id,
                });
            }
            IRInstruction::GetIterator(kind) => {
                self.visit_get_iterator(&GetIterator {
                    ctx: parent.ctx(),
                    node: node,
                    node_id: node_id,
                    kind: &kind,
                });
            }
            _ => {
                panic!(
                    "Not implemented - visit_value: {:#?}",
                    parent.ctx().graph().get_node(node_id).instruction()
                );
            }
        }
    }

    fn visit_end(&mut self, end: &End) {
        self.walk_end(end);
    }

    fn walk_end(&mut self, end: &End) {
        enter_node!(self, end);
        enter_node!(self, end);
    }

    fn visit_loop_head(&mut self, r#loop: &Loop) {
        self.walk_loop_head(r#loop);
    }

    fn walk_loop_head(&mut self, r#loop: &Loop) {
        enter_node!(self, r#loop);

        self.visit_control_outputs(r#loop);

        enter_node!(self, r#loop);
    }

    fn visit_loop_end(&mut self, r#loop: &Loop) {
        self.walk_loop_end(r#loop);
    }

    fn walk_loop_end(&mut self, r#loop: &Loop) {
        enter_node!(self, r#loop);
        enter_node!(self, r#loop);
    }

    fn visit_proj_control(&mut self, proj: &Proj) {
        self.walk_proj_control(proj);
    }

    fn walk_proj_control(&mut self, proj: &Proj) {
        enter_node!(self, proj);

        // Visit all children as statements:
        self.visit_control_outputs(proj);

        enter_node!(self, proj);
    }

    fn visit_proj_value(&mut self, proj: &Proj) {
        self.walk_proj_value(proj);
    }

    fn walk_proj_value(&mut self, proj: &Proj) {
        enter_node!(self, proj);

        // Visit all children as statements:
        self.visit_value(proj, proj.value_id());

        enter_node!(self, proj);
    }

    fn visit_if_else(&mut self, if_else: &IfElse) {
        self.walk_if_else(if_else);
    }

    fn walk_if_else(&mut self, if_else: &IfElse) {
        enter_node!(self, if_else);

        // 1. Visit condition.
        let condition_id = if_else.condition_id();
        self.visit_value(if_else, condition_id);

        // 2. Visit consequent.
        let consequent_id = if_else.consequent_id();
        self.visit_control(if_else, consequent_id);

        // 3. Visit alternate.
        let alternate_id = if_else.alternate_id();
        self.visit_control(if_else, alternate_id);

        // 4. Visit post-merge continuation.
        if let Some(cont_id) = if_else.continuation_id() {
            self.visit_control(if_else, cont_id);
        }

        leave_node!(self, if_else);
    }

    fn visit_merge(&mut self, merge: &Merge) {
        self.walk_merge(merge);
    }

    fn walk_merge(&mut self, merge: &Merge) {
        enter_node!(self, merge);

        match merge.phi_index() {
            Some(phi_input_index) => {
                for output_id in merge.node.outputs() {
                    let phi_id = *output_id;
                    let phi_node = merge.ctx().graph().get_node(phi_id);
                    if *phi_node.instruction() == IRInstruction::Phi {
                        self.visit_phi_control(&PhiControl {
                            ctx: merge.ctx(),
                            node: phi_node,
                            node_id: phi_id,
                            phi_input_index,
                        });
                    }
                }
            }
            None => {
                // This is a continuation of control flow.
                self.visit_control_outputs(merge);
            }
        }

        leave_node!(self, merge);
    }

    fn visit_bind_export(&mut self, bind_export: &BindExport) {
        self.walk_bind_export(bind_export);
    }

    fn walk_bind_export(&mut self, bind_export: &BindExport) {
        enter_node!(self, bind_export);

        self.visit_value(bind_export, bind_export.value_id());

        enter_node!(self, bind_export);
    }

    fn visit_return(&mut self, ret: &Return) {
        self.walk_return(ret);
    }

    fn walk_return(&mut self, ret: &Return) {
        enter_node!(self, ret);

        self.visit_value(ret, ret.value_id());

        leave_node!(self, ret);
    }

    fn visit_phi_control(&mut self, phi_control: &PhiControl) {
        self.walk_phi_control(phi_control);
    }

    fn walk_phi_control(&mut self, phi_control: &PhiControl) {
        enter_node!(self, phi_control);

        let value_id = phi_control.value_input_id();
        self.visit_value(phi_control, value_id);

        leave_node!(self, phi_control);
    }

    fn visit_constant(&mut self, constant: &Constant) {
        self.walk_constant(constant);
    }

    fn walk_constant(&mut self, constant: &Constant) {
        enter_node!(self, constant);
        enter_node!(self, constant);
    }

    fn visit_load_global(&mut self, load_global: &LoadGlobal) {
        self.walk_load_global(load_global);
    }

    fn walk_load_global(&mut self, load_global: &LoadGlobal) {
        enter_node!(self, load_global);
        enter_node!(self, load_global);
    }

    fn visit_phi_value(&mut self, phi_value: &PhiValue) {
        self.walk_phi_value(phi_value);
    }

    fn walk_phi_value(&mut self, phi_value: &PhiValue) {
        enter_node!(self, phi_value);
        enter_node!(self, phi_value);
    }

    fn visit_add(&mut self, add: &Add) {
        self.walk_add(add);
    }

    fn walk_add(&mut self, add: &Add) {
        enter_node!(self, add);
        enter_node!(self, add);
    }

    fn visit_compare(&mut self, compare: &Compare) {
        self.walk_compare(compare);
    }

    fn walk_compare(&mut self, compare: &Compare) {
        enter_node!(self, compare);

        self.visit_value(compare, compare.left_id());
        self.visit_value(compare, compare.right_id());

        enter_node!(self, compare);
    }

    fn visit_get_iterator(&mut self, get_iterator: &GetIterator) {
        self.walk_get_iterator(get_iterator);
    }

    fn walk_get_iterator(&mut self, get_iterator: &GetIterator) {
        enter_node!(self, get_iterator);

        self.visit_value(get_iterator, get_iterator.value_id());

        enter_node!(self, get_iterator);
    }

    fn visit_iterator_next(&mut self, iterator_next: &IteratorNext) {
        self.walk_iterator_next(iterator_next);
    }

    fn walk_iterator_next(&mut self, iterator_next: &IteratorNext) {
        enter_node!(self, iterator_next);

        self.visit_value(iterator_next, iterator_next.value_id());

        enter_node!(self, iterator_next);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use oxsea_ir::CompileTimeValue::*;
    use oxsea_ir::IteratorKind;
    use oxsea_ir::IR_START_ID;

    struct NoopVisit {
        node_ids: std::vec::Vec<IRNodeId>,
    }

    impl Visit for NoopVisit {
        fn enter_node(&mut self, node_id: IRNodeId, _node: &IRNode) {
            self.node_ids.push(node_id);
        }
    }

    #[test]
    fn traverse_with_cycle() {
        let mut ir = IRGraph::new();

        let init_sum = ir.add_constant(Number(0.0));
        let arr = ir.add_load_global("arr".to_string());
        let init_iter_record = ir.add_get_iterator(arr, IteratorKind::Sync);
        let for_of_loop = ir.add_loop(IR_START_ID);
        let sum = ir.add_phi(for_of_loop, &[init_sum]);
        let iter_record = ir.add_phi(for_of_loop, &[init_iter_record]);
        let next = ir.add_iterator_next(iter_record);
        let next_iter_record = ir.add_proj(next, 0);
        let next_done = ir.add_proj(next, 1);
        let next_value = ir.add_proj(next, 2);
        let loop_exit = ir.add_if_else(for_of_loop, next_done);
        let loop_exit_true = ir.add_proj(loop_exit, 0);
        let loop_exit_false = ir.add_proj(loop_exit, 1);
        let sum_plus_value = ir.add_add(sum, next_value);
        ir.add_edge(loop_exit_true, IR_END_ID);
        ir.add_edge(loop_exit_false, for_of_loop);
        ir.add_edge(sum_plus_value, sum);
        ir.add_edge(next_iter_record, iter_record);
        let mut visit = NoopVisit { node_ids: vec![] };
        traverse(&ir, &mut visit);
        assert!(visit.node_ids.contains(&IR_END_ID), "End node not visited");
    }
}
