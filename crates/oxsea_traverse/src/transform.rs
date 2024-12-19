use oxsea_ir::{IRGraph, IRInstruction, IRNodeId, IR_END_ID, IR_START_ID};

use crate::{sort::reverse_topological_sort, Constant, Return};

pub trait Transform {
    fn visit_constant(&mut self, constant: &Constant, out: &mut IRGraph) -> IRNodeId {
        self.transform_constant(constant, out)
            .unwrap_or_else(|| out.add_constant(constant.value().clone()))
    }

    fn transform_constant(&mut self, _constant: &Constant, _out: &mut IRGraph) -> Option<IRNodeId> {
        None
    }

    fn visit_return(&mut self, ret: &Return, out: &mut IRGraph) -> IRNodeId {
        self.transform_return(ret, out)
            .unwrap_or_else(|| out.add_return(ret.control_id(), ret.value_id()))
    }

    fn transform_return(&mut self, _ret: &Return, _out: &mut IRGraph) -> Option<IRNodeId> {
        None
    }
}

pub fn transform<T>(ir: &IRGraph, transform: &mut T) -> IRGraph
where
    T: Transform,
{
    let order = reverse_topological_sort(ir);
    let mut context = crate::Context::new_with_ids(ir);

    let mut out = IRGraph::new();

    for node_id in order {
        if node_id == IR_START_ID || node_id == IR_END_ID {
            continue;
        }

        let node = ir.get_node(node_id);
        let new_node_id = match node.instruction() {
            IRInstruction::Constant(value) => transform.visit_constant(
                &Constant {
                    ctx: &context,
                    node_id,
                    node,
                    value: value,
                },
                &mut out,
            ),
            IRInstruction::Return => transform.visit_return(
                &Return {
                    ctx: &context,
                    node_id,
                    node,
                },
                &mut out,
            ),
            _ => {
                panic!("Unhandled instruction: {:?}", node.instruction());
            }
        };
        context.ids[node_id] = new_node_id;
    }

    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use oxsea_ir::CompileTimeValue::*;
    use oxsea_ir::IR_START_ID;

    struct CopyTransform {}

    impl Transform for CopyTransform {}

    #[test]
    fn copy_add_with_constants() {
        let mut ir = IRGraph::new();
        let left = ir.add_constant(Number(1.0));
        let right = ir.add_constant(Number(2.0));
        let add = ir.add_add(left, right);
        ir.add_return(IR_START_ID, add);
        let mut copy_transform = CopyTransform {};
        let out = transform(&ir, &mut copy_transform);
        assert!(out.len() == 4);
    }
}
