pub mod delegate;
pub mod entity;
pub mod obj;
mod util;
pub use autoken;

pub mod prelude {
    pub use super::{
        delegate::delegate,
        entity::{CyclicCtor, Entity, StrongEntity},
        obj::{Obj, ObjMut, ObjRef, StrongObj},
    };
}

pub use prelude::*;
