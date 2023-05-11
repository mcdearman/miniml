use std::mem;

const WORD_SIZE: usize = mem::size_of::<usize>();

fn align(size: usize) -> usize {
    (size + WORD_SIZE - 1) & !(WORD_SIZE - 1)
}

struct GarbageCollector {
    from_space: *mut u8,
    from_space_end: *mut u8,
    from_space_top: *mut u8,
    to_space: *mut u8,
    to_space_end: *mut u8,
    to_space_top: *mut u8,
    roots: *mut *mut u8,
    roots_end: *mut *mut u8,
}

impl GarbageCollector {
    fn new(
        from_space: *mut u8,
        from_space_size: usize,
        to_space: *mut u8,
        to_space_size: usize,
        roots: *mut *mut u8,
        roots_size: usize,
    ) -> GarbageCollector {
        GarbageCollector {
            from_space,
            from_space_end: unsafe { from_space.add(from_space_size) },
            from_space_top: from_space,
            to_space,
            to_space_end: unsafe { to_space.add(to_space_size) },
            to_space_top: to_space,
            roots,
            roots_end: unsafe { roots.add(roots_size) },
        }
    }

    fn gc_malloc(&mut self, size: usize) -> *mut u8 {
        let total_size = align(size);
        if unsafe { self.from_space_top.add(total_size) > self.from_space_end } {
            println!("Running GC");
            // No more space in the from-space, run GC
            self.gc();
            if unsafe { self.from_space_top.add(total_size) > self.from_space_end } {
                // Cannot allocate the requested memory
                return std::ptr::null_mut();
            }
        }
        let obj = self.from_space_top;
        self.from_space_top = unsafe { self.from_space_top.add(total_size) };
        unsafe { obj.copy_from(self.to_space, size) };
        obj
    }

    fn scan(&mut self, obj: *mut u8) {
        let mut p = obj as *mut *mut u8;
        while p < unsafe { (obj as *mut *mut u8).add(align(*(obj as *const usize))) } {
            let p_val = unsafe { *p };
            if self.from_space <= p_val
                && p_val < self.from_space_end
                && !(self.to_space <= p_val && p_val < self.to_space_end)
            {
                // Found a pointer to a live object in the from-space
                let offset = unsafe { p_val.offset_from(self.from_space) as usize };
                let new_p_val = unsafe { self.to_space.add(offset) };
                unsafe { *p = new_p_val };
                if new_p_val > self.to_space_top {
                    self.to_space_top = new_p_val;
                }
            }
            p = unsafe { p.add(1) };
        }
    }

    fn scan_roots(&mut self) {
        let mut p = self.roots;
        while p < self.roots_end {
            let p_val = unsafe { *p };
            if self.from_space <= p_val
                && p_val < self.from_space_end
                && !(self.to_space <= p_val && p_val < self.to_space_end)
            {
                // Found a pointer to a live object in the from-space
                let offset = unsafe { p_val.offset_from(self.from_space) as usize };
                let new_p_val = unsafe { self.to_space.add(offset) };
                unsafe { *p = new_p_val };
                if new_p_val > self.to_space_top {
                    self.to_space_top = new_p_val;
                }
            }
            p = unsafe { p.add(1) };
        }
    }

    fn gc(&mut self) {
        std::mem::swap(&mut self.from_space, &mut self.to_space);
        std::mem::swap(&mut self.from_space_end, &mut self.to_space_end);
        self.from_space_top = self.from_space;
        self.to_space_top = self.to_space;

        let mut p = self.roots;
        while p < self.roots_end {
            let obj = unsafe { *p };
            self.scan(obj);
            p = unsafe { p.add(1) };
        }

        while self.to_space_top < self.to_space_end {
            let obj = self.from_space_top;
            let size = align(unsafe { *(obj as *const usize) });
            self.from_space_top = unsafe { self.from_space_top.add(size) };
            self.scan(obj);
        }
    }
}

mod tests {
    #[test]
    fn test_gc_malloc() {
        use super::GarbageCollector;
        use std::mem;

        let mut gc = GarbageCollector::new(
            &mut [0u8; 1024] as *mut [u8] as *mut u8,
            1024,
            &mut [0u8; 1024] as *mut [u8] as *mut u8,
            1024,
            &mut [0usize; 1024] as *mut [usize] as *mut *mut u8,
            1024,
        );

        let obj = gc.gc_malloc(mem::size_of::<usize>());
        assert!(!obj.is_null());
        assert_eq!(obj as usize % mem::size_of::<usize>(), 0);
    }

    #[test]
    fn test_gc_malloc_multiple() {
        use super::GarbageCollector;
        use std::mem;

        let mut gc = GarbageCollector::new(
            &mut [0u8; 1024] as *mut [u8] as *mut u8,
            1024,
            &mut [0u8; 1024] as *mut [u8] as *mut u8,
            1024,
            &mut [0usize; 1024] as *mut [usize] as *mut *mut u8,
            1024,
        );

        let obj1 = gc.gc_malloc(mem::size_of::<usize>());
        assert!(!obj1.is_null());
        assert_eq!(obj1 as usize % mem::size_of::<usize>(), 0);

        let obj2 = gc.gc_malloc(mem::size_of::<usize>());
        assert!(!obj2.is_null());
        assert_eq!(obj2 as usize % mem::size_of::<usize>(), 0);

        assert_ne!(obj1, obj2);
    }
}
