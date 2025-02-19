struct Lazy<T> {
    phantom: std::marker::PhantomData<T>,
}

impl<T> Lazy<T> {
    fn delay<F>(f: F) -> Self
    where
        F: FnOnce() -> T,
    {
        unimplemented!()
    }

    fn force(&self) -> &T {
        unimplemented!()
    }
}
