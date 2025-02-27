#[macro_export]
macro_rules! enter_node {
    ( $self:ident, $node:expr ) => {
        $self.enter_node($node.node_id, $node.node)
    };
}

#[macro_export]
macro_rules! leave_node {
    ( $self:ident, $node:expr ) => {
        $self.leave_node($node.node_id, $node.node)
    };
}

#[macro_export]
macro_rules! node_kind {
    ( $node:ident ) => {
        impl<'i> IRNodeKind<'i> for $node<'i> {
            fn ctx(&self) -> &'i Context<'i> {
                self.ctx
            }
            fn node(&self) -> &'i IRNode {
                self.node
            }
            fn node_id(&self) -> IRNodeId {
                self.node_id
            }
        }
    };
}

#[macro_export]
macro_rules! input_id_accessor {
    ($name:ident, $idx:expr) => {
        pub fn $name(&self) -> IRNodeId {
            self.node.inputs()[$idx]
        }
    };
}

#[macro_export]
macro_rules! output_id_accessor {
    ($name:ident, $idx:expr) => {
        pub fn $name(&self) -> IRNodeId {
            self.node.outputs()[$idx]
        }
    };
}

#[macro_export]
macro_rules! optional_output_id_accessor {
    ($name:ident, $idx:expr) => {
        pub fn $name(&self) -> Option<IRNodeId> {
            self.node.outputs().get($idx).copied()
        }
    };
}
