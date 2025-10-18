//! Index types. See [`::index_vec`].

pub use index_vec::{
    Idx, IdxRangeBounds, IdxSliceIndex, IndexBox, IndexSlice, IndexVec, index_box, index_vec,
};

pub trait GudIndex:
    Copy
    + PartialEq
    + Eq
    + PartialOrd
    + Ord
    + core::hash::Hash
    + Idx
    + std::ops::Add<u32, Output = Self>
    + std::ops::Sub<Self, Output = u32>
{
    const ZERO: Self;

    fn get(self) -> u32;

    fn get_and_inc(&mut self) -> Self;

    fn iter_to(self, to: Self) -> impl Iterator<Item = Self>;
}

pub fn iter_idx<I: GudIndex>(r: std::ops::Range<I>) -> impl Iterator<Item = I> {
    r.start.iter_to(r.end)
}

/// Creates a new index to use with [`::index_vec`].
#[macro_export]
macro_rules! newtype_index {
    () => {};
    ($(#[$attr:meta])* $vis:vis struct $name:ident; $($rest:tt)*) => {
        newtype_index!($(#[$attr])* $vis struct $name $vis new; $($rest)*);
    };
    ($(#[$attr:meta])* $vis:vis struct $name:ident $new_vis:vis new; $($rest:tt)*) => {
        $(#[$attr])*
        #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        #[repr(transparent)]
        $vis struct $name(std::num::NonZero<u32>);

        impl std::fmt::Debug for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}({:?})", stringify!($name), self.get())
            }
        }

        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", self.get())
            }
        }

        impl $crate::index::Idx for $name {
            #[inline(always)]
            fn from_usize(value: usize) -> Self {
                let value = u32::try_from(value).expect("index overflowed");
                Self::new(value)
            }

            #[inline(always)]
            fn index(self) -> usize {
                self.get() as usize
            }
        }

        impl $name {
            /// Creates a new `$name` from the given `value`.
            #[inline(always)]
            $new_vis const fn new(value: u32) -> Self {
                let inner_repr = value.checked_add(1).expect("index overflowed");
                Self(std::num::NonZero::new(inner_repr).expect("inner_repr should never be zero"))
            }

            /// Gets the underlying index value.
            #[inline(always)]
            $vis const fn get(self) -> u32 {
                self.0.get() - 1
            }

            #[inline(always)]
            $vis fn get_and_inc(&mut self) -> Self {
                let current = *self;
                *self = current + 1;
                current
            }
        }

        impl $crate::index::GudIndex for $name {
            const ZERO: Self = Self::new(0);

            fn get(self) -> u32 {
                self.get()
            }

            fn get_and_inc(&mut self) -> Self {
                self.get_and_inc()
            }

            fn iter_to(self, to: Self) -> impl Iterator<Item = Self> {
                let index = <Self as $crate::Idx>::index;
                (index(self)..index(to)).map($crate::Idx::from_usize)
            }
        }

        impl std::ops::Add<u32> for $name {
            type Output = Self;
            fn add(self, rhs: u32) -> Self::Output {
                Self::new(self.get().checked_add(rhs).expect("index addition overflowed"))
            }
        }

        impl std::ops::Sub<Self> for $name {
            type Output = u32;
            fn sub(self, rhs: Self) -> Self::Output {
                self.get() - rhs.get()
            }
        }

        $crate::newtype_index!($($rest)*);
    };
}

newtype_index! {
    pub struct CasesBasicBlocksIndex;
    pub struct FunctionId;
    pub struct BasicBlockId;
    pub struct OperationIndex;
    pub struct DataId;
    pub struct DataOffset;
    pub struct LocalId;
    pub struct LocalIndex;
    pub struct LargeConstId;
    pub struct CasesId;
    pub struct StaticAllocId;
}

pub struct IndexLinearSet<I: Idx, V: PartialEq> {
    inner: IndexVec<I, V>,
}

impl<I: Idx, V: PartialEq> IndexLinearSet<I, V> {
    pub fn new() -> Self {
        Self { inner: IndexVec::new() }
    }

    pub fn with_capacity(size: usize) -> Self {
        Self { inner: IndexVec::with_capacity(size) }
    }

    pub fn add(&mut self, value: V) -> Result<I, I> {
        self.find(&value).map_or(Ok(()), |i| Err(i))?;
        let new_id = self.len_idx();
        self.inner.push(value);
        Ok(new_id)
    }

    pub fn find(&self, value: &V) -> Option<I> {
        self.position(|member| member == value)
    }
}

impl<I: Idx, V: PartialEq> Default for IndexLinearSet<I, V> {
    fn default() -> Self {
        Self::new()
    }
}

impl<I: Idx, V: PartialEq> std::ops::Deref for IndexLinearSet<I, V> {
    type Target = IndexVec<I, V>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    newtype_index!(
        struct MyIndex;
    );

    #[test]
    fn test_newtype_index() {
        assert_eq!(MyIndex::new(0).get(), 0);
        assert_eq!(MyIndex::new(1).get(), 1);
        assert_eq!(MyIndex::new(0xFFFF_FF00).get(), 0xFFFF_FF00);
    }

    #[test]
    fn test_index_size() {
        assert_eq!(std::mem::size_of::<MyIndex>(), 4);
        assert_eq!(std::mem::size_of::<Option<MyIndex>>(), 4);
        assert_eq!(std::mem::size_of::<BasicBlockId>(), 4);
        assert_eq!(std::mem::size_of::<Option<BasicBlockId>>(), 4);
        assert_eq!(std::mem::size_of::<OperationIndex>(), 4);
        assert_eq!(std::mem::size_of::<Option<OperationIndex>>(), 4);
        assert_eq!(std::mem::size_of::<DataOffset>(), 4);
        assert_eq!(std::mem::size_of::<Option<DataOffset>>(), 4);
        assert_eq!(std::mem::size_of::<LocalId>(), 4);
        assert_eq!(std::mem::size_of::<Option<LocalId>>(), 4);
        assert_eq!(std::mem::size_of::<LocalIndex>(), 4);
    }
}
