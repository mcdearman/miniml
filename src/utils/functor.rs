pub trait Functor {
    type Wrapped<T: TBounds>: WBounds; // Acts as a higher-kinded type

    fn fmap<F, T, U>(f: F, x: Self::Wrapped<T>) -> Self::Wrapped<U>
    where
        F: FnOnce(T) -> U;
}
