#[derive(Debug, Clone, PartialEq)]
pub enum FingerTree<T> {
    Empty,
    Single(T),
    Deep(Digit<T>, Box<FingerTree<Node<T>>>, Digit<T>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Digit<T> {
    One(T),
    Two(T, T),
    Three(T, T, T),
    Four(T, T, T, T),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Node<T> {
    Node2(T, T),
    Node3(T, T, T),
}

// data FingerTree a
//   = Empty
//   | Single a
//   | Deep (Digit a) (FingerTree (Node a)) (Digit a)

// data Node a
//   = Node2 a a
//   | Node3 a a a

// type Digit a = One a | Two a a | Three a a a | Four a a a a
