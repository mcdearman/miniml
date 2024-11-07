pub trait Functor {
    type Wrapped<T: Clone>: Clone; // Acts as a higher-kinded type

    fn map<F, T: Clone, U: Clone>(&self, f: F, x: Self::Wrapped<T>) -> Self::Wrapped<U>
    where
        F: FnOnce(T) -> U;
}
