use std::fmt;

#[derive(Clone)]
pub enum Frag {
    Branch(Box<dyn Parent>),
    Leaf(String),
}

impl PartialEq for Frag {
    fn eq(&self, other: &Self) -> bool {
        true
    }
}

impl fmt::Display for Frag {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Frag::Branch(p) => write!(f, "{}", p),
            Frag::Leaf(l) => write!(f, "{}", l),
        }
    }
}

pub trait Parent: ParentClone + fmt::Display {
    fn children(&self) -> Vec<Frag>;
}

trait ParentClone {
    fn clone_box(&self) -> Box<dyn Parent>;
}

impl<T> ParentClone for T
where
    T: 'static + Parent + Clone,
{
    fn clone_box(&self) -> Box<dyn Parent> {
        Box::new(self.clone())
    }
}

impl Clone for Box<dyn Parent> {
    fn clone(&self) -> Box<dyn Parent> {
        self.clone_box()
    }
}
