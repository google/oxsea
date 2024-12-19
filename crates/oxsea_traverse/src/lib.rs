use oxc_ast::ast::BinaryOperator;
use oxsea_ir::{
    CompileTimeValue, IRGraph, IRInstruction, IRNode, IRNodeId, IR_END_ID, IR_INVALID_ID,
    IR_START_ID,
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
    pub fn value_id(&self) -> IRNodeId {
        self.node.inputs()[1]
    }

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
}
node_kind! { IfElse }

impl IfElse<'_> {
    pub fn condition_id(&self) -> IRNodeId {
        self.node.inputs()[1]
    }

    pub fn consequent_id(&self) -> IRNodeId {
        self.node.outputs()[0]
    }

    pub fn alternate_id(&self) -> IRNodeId {
        self.node.outputs()[1]
    }
}

pub struct Block<'i> {
    ctx: &'i Context<'i>,
    node: &'i IRNode,
    node_id: IRNodeId,
}
node_kind! { Block }

pub struct Merge<'i> {
    ctx: &'i Context<'i>,
    node: &'i IRNode,
    node_id: IRNodeId,
}
node_kind! { Merge }

pub struct PhiControl<'i> {
    ctx: &'i Context<'i>,
    node: &'i IRNode,
    node_id: IRNodeId,

    phi_input_index: usize,
}
node_kind! { PhiControl }

impl PhiControl<'_> {
    pub fn control_input_id(&self) -> usize {
        self.node.inputs()[self.phi_input_index]
    }

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
        match node.instruction() {
            IRInstruction::Block => {
                self.visit_block(&Block {
                    ctx: parent.ctx(),
                    node: node,
                    node_id,
                });
            }
            IRInstruction::IfElse => {
                self.visit_if_else(&IfElse {
                    ctx: parent.ctx(),
                    node: node,
                    node_id,
                });
            }
            // IRInstruction::Merge => {
            //     self.visit_merge(parent_id, node_id, ctx);
            // }
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
            IRInstruction::Phi => {
                let parent_id = parent.node_id();
                let phi_input_index = node.inputs().iter().position(|x| *x == parent_id);
                if let Some(phi_input_index) = phi_input_index {
                    self.visit_phi_control(&PhiControl {
                        ctx: parent.ctx(),
                        node: node,
                        node_id,
                        phi_input_index,
                    });
                }
            }
            IRInstruction::End => {
                self.visit_end(&End {
                    ctx: parent.ctx(),
                    node: node,
                    node_id,
                });
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

    fn visit_block(&mut self, block: &Block) {
        self.walk_block(block);
    }

    fn walk_block(&mut self, block: &Block) {
        enter_node!(self, block);

        // Visit all children as statements:
        self.visit_control_outputs(block);

        enter_node!(self, block);
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

        leave_node!(self, if_else);
    }

    fn visit_merge(&mut self, parent_id: IRNodeId, node_id: IRNodeId, ctx: &Context) {
        self.walk_merge(parent_id, node_id, ctx);
    }

    fn walk_merge(&mut self, _parent_id: IRNodeId, node_id: IRNodeId, ctx: &Context) {
        self.enter_node(node_id, ctx.graph().get_node(node_id));

        // Visit all children as statements:
        // self.visit_control_outputs(node_id, ctx);

        self.leave_node(node_id, ctx.graph().get_node(node_id));
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
}
