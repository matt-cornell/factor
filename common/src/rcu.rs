use std::fmt::{self, Debug, Formatter};
use std::mem::ManuallyDrop;
use std::sync::atomic::{AtomicPtr, Ordering};
use triomphe::Arc;

/// A read-copy-update pointer
pub struct Rcu<T> {
    inner: AtomicPtr<T>,
}
impl<T> Rcu<T> {
    pub fn new(arc: Arc<T>) -> Self {
        Self {
            inner: AtomicPtr::new(Arc::into_raw(arc) as _),
        }
    }
    pub fn read(&self) -> Arc<T> {
        let ptr = self.inner.load(Ordering::Acquire);
        unsafe { Arc::clone(&ManuallyDrop::new(Arc::from_raw(ptr))) }
    }
    pub fn write(&self, new: Arc<T>) -> Arc<T> {
        let old_ptr = self.inner.swap(Arc::into_raw(new) as _, Ordering::Release);
        unsafe { Arc::clone(&ManuallyDrop::new(Arc::from_raw(old_ptr))) }
    }
    pub fn update<R, F: FnOnce(&mut T) -> R>(&self, up: F) -> R
    where
        T: Clone,
    {
        let mut val = T::clone(&self.read());
        let res = up(&mut val);
        self.write(Arc::new(val));
        res
    }
    pub fn read_mut(&mut self) -> Arc<T> {
        let ptr = self.inner.get_mut();
        unsafe { Arc::clone(&ManuallyDrop::new(Arc::from_raw(*ptr))) }
    }
    pub fn write_mut(&mut self, new: Arc<T>) -> Arc<T> {
        let old_ptr = std::mem::replace(self.inner.get_mut(), Arc::into_raw(new) as _);
        unsafe { Arc::clone(&ManuallyDrop::new(Arc::from_raw(old_ptr))) }
    }
    pub fn update_mut<R, F: FnOnce(&mut T) -> R>(&mut self, up: F) -> R
    where
        T: Clone,
    {
        unsafe {
            let ptr = self.inner.get_mut();
            let mut arc = ManuallyDrop::new(Arc::from_raw(*ptr));
            if let Some(inner) = Arc::get_mut(&mut arc) {
                up(inner)
            } else {
                let mut inner = T::clone(&arc);
                let res = up(&mut inner);
                ManuallyDrop::drop(&mut arc);
                *ptr = Arc::into_raw(Arc::new(inner)) as _;
                res
            }
        }
    }
}
impl<T: Debug> Debug for Rcu<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("Rcu").field("inner", &self.read()).finish()
    }
}
impl<T: Default> Default for Rcu<T> {
    fn default() -> Self {
        Self::new(Arc::default())
    }
}
impl<T> Drop for Rcu<T> {
    fn drop(&mut self) {
        unsafe {
            let _ = Arc::from_raw(*self.inner.get_mut());
        }
    }
}

/// A read-copy-update pointer that can be `None`.
pub struct OptionRcu<T> {
    inner: AtomicPtr<T>,
}
impl<T> OptionRcu<T> {
    pub const fn none() -> Self {
        Self {
            inner: AtomicPtr::new(std::ptr::null_mut()),
        }
    }
    pub fn new(arc: Option<Arc<T>>) -> Self {
        Self {
            inner: AtomicPtr::new(arc.map_or_else(std::ptr::null, Arc::into_raw) as _),
        }
    }
    pub fn new_some(arc: Arc<T>) -> Self {
        Self {
            inner: AtomicPtr::new(Arc::into_raw(arc) as _),
        }
    }
    pub fn read(&self) -> Option<Arc<T>> {
        let ptr = self.inner.load(Ordering::Acquire);
        if ptr.is_null() {
            None
        } else {
            unsafe { Some(Arc::clone(&ManuallyDrop::new(Arc::from_raw(ptr)))) }
        }
    }
    pub fn write(&self, new: Option<Arc<T>>) -> Option<Arc<T>> {
        let old_ptr = self.inner.swap(
            new.map_or_else(std::ptr::null, Arc::into_raw) as _,
            Ordering::Release,
        );
        if old_ptr.is_null() {
            None
        } else {
            unsafe { Some(Arc::clone(&ManuallyDrop::new(Arc::from_raw(old_ptr)))) }
        }
    }
    pub fn update<R, F: FnOnce(&mut Option<T>) -> R>(&self, up: F) -> R
    where
        T: Clone,
    {
        let mut val = self.read().as_deref().cloned();
        let res = up(&mut val);
        self.write(val.map(Arc::new));
        res
    }
    pub fn read_mut(&mut self) -> Option<Arc<T>> {
        let ptr = self.inner.get_mut();
        if ptr.is_null() {
            None
        } else {
            unsafe { Some(Arc::clone(&ManuallyDrop::new(Arc::from_raw(*ptr)))) }
        }
    }
    pub fn write_mut(&mut self, new: Option<Arc<T>>) -> Option<Arc<T>> {
        let old_ptr = std::mem::replace(
            self.inner.get_mut(),
            new.map_or_else(std::ptr::null, Arc::into_raw) as _,
        );
        if old_ptr.is_null() {
            None
        } else {
            unsafe { Some(Arc::clone(&ManuallyDrop::new(Arc::from_raw(old_ptr)))) }
        }
    }
    pub fn update_mut<R, F: FnOnce(&mut T) -> R>(&mut self, up: F) -> R
    where
        T: Clone,
    {
        unsafe {
            let ptr = self.inner.get_mut();
            let mut arc = ManuallyDrop::new(Arc::from_raw(*ptr));
            if let Some(inner) = Arc::get_mut(&mut arc) {
                up(inner)
            } else {
                let mut inner = T::clone(&arc);
                let res = up(&mut inner);
                ManuallyDrop::drop(&mut arc);
                *ptr = Arc::into_raw(Arc::new(inner)) as _;
                res
            }
        }
    }
}
impl<T: Debug> Debug for OptionRcu<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("Rcu").field("inner", &self.read()).finish()
    }
}
impl<T: Default> Default for OptionRcu<T> {
    fn default() -> Self {
        Self::new(None)
    }
}
impl<T> Drop for OptionRcu<T> {
    fn drop(&mut self) {
        unsafe {
            let ptr = *self.inner.get_mut();
            if !ptr.is_null() {
                let _ = Arc::from_raw(ptr);
            }
        }
    }
}
