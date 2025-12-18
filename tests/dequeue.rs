use std::ptr::NonNull;

type Link<T> = Option<NonNull<Node<T>>>;

pub struct List<T> {
	head: Link<T>,
	tail: Link<T>,
}

// Node doesn't need to implement Drop because memory is managed by Box through
// List. When a node is removed via pop_front/pop_back, Box::from_raw converts
// the raw pointer back to a Box, which automatically drops the node and its
// fields when it goes out of scope.
struct Node<T> {
	data: T,
	next: Link<T>,
	prev: Link<T>,
}

impl<T> Node<T> {
	fn new(data: T) -> NonNull<Self> {
		unsafe { NonNull::new_unchecked(Box::into_raw(Box::new(Node { data, prev: None, next: None }))) }
	}
}

impl<T> List<T> {
	pub fn new() -> Self {
		List { head: None, tail: None }
	}

	pub fn push_front(&mut self, data: T) {
		let mut new_head = Node::new(data);
		if let Some(mut old_head) = self.head.take() {
			unsafe { old_head.as_mut() }.prev = Some(new_head);
			unsafe { new_head.as_mut() }.next = Some(old_head);
		} else {
			self.tail = Some(new_head);
		}
		self.head = Some(new_head);
	}

	pub fn push_back(&mut self, data: T) {
		let mut new_tail = Node::new(data);
		if let Some(mut old_tail) = self.tail.take() {
			unsafe { old_tail.as_mut() }.next = Some(new_tail);
			unsafe { new_tail.as_mut() }.prev = Some(old_tail);
		} else {
			self.head = Some(new_tail);
		}
		self.tail = Some(new_tail);
	}

	pub fn pop_back(&mut self) -> Option<T> {
		self.tail.take().map(|mut old_tail| {
			if let Some(mut new_tail) = unsafe { old_tail.as_mut() }.prev.take() {
				unsafe { new_tail.as_mut() }.next.take();
				self.tail = Some(new_tail);
			} else {
				self.head.take();
			}
			unsafe { Box::from_raw(old_tail.as_ptr()) }.data
		})
	}

	pub fn pop_front(&mut self) -> Option<T> {
		self.head.take().map(|mut old_head| {
			if let Some(mut new_head) = unsafe { old_head.as_mut() }.next.take() {
				unsafe { new_head.as_mut() }.prev.take();
				self.head = Some(new_head);
			} else {
				self.tail.take();
			}
			unsafe { Box::from_raw(old_head.as_ptr()) }.data
		})
	}

	pub fn peek_front(&self) -> Option<&T> {
		self.head.as_ref().map(|node| unsafe { &node.as_ref().data })
	}

	pub fn peek_back(&self) -> Option<&T> {
		self.tail.as_ref().map(|node| unsafe { &node.as_ref().data })
	}

	pub fn peek_back_mut(&mut self) -> Option<&mut T> {
		self.tail.as_ref().map(|node| unsafe { &mut (*node.as_ptr()).data })
	}

	pub fn peek_front_mut(&mut self) -> Option<&mut T> {
		self.head.as_ref().map(|node| unsafe { &mut (*node.as_ptr()).data })
	}

	pub(crate) fn into_iter(self) -> IntoIter<T> {
		IntoIter(self)
	}
}

impl<T> Default for List<T> {
	fn default() -> Self {
		Self::new()
	}
}

impl<T> Drop for List<T> {
	fn drop(&mut self) {
		while self.pop_front().is_some() {}
	}
}

pub struct IntoIter<T>(List<T>);

impl<T> Iterator for IntoIter<T> {
	type Item = T;

	fn next(&mut self) -> Option<T> {
		self.0.pop_front()
	}
}

impl<T> DoubleEndedIterator for IntoIter<T> {
	fn next_back(&mut self) -> Option<T> {
		self.0.pop_back()
	}
}

#[cfg(test)]
mod test {
	use std::rc::Rc;

	use super::List;

	#[test]
	fn basics() {
		let mut list = List::new();

		// Check empty list behaves right
		assert_eq!(list.pop_front(), None);
		assert_eq!(list.pop_back(), None);

		// Populate list
		list.push_front(1);
		list.push_front(2);
		list.push_front(3);

		// Check normal removal
		assert_eq!(list.pop_front(), Some(3));
		assert_eq!(list.pop_front(), Some(2));

		// Push some more just to make sure nothing's corrupted
		list.push_front(4);
		list.push_front(5);

		// Check normal removal
		assert_eq!(list.pop_front(), Some(5));
		assert_eq!(list.pop_front(), Some(4));

		// Check exhaustion
		assert_eq!(list.pop_front(), Some(1));
		assert_eq!(list.pop_front(), None);

		// ---- back -----

		// Check empty list behaves right
		assert_eq!(list.pop_back(), None);

		// Populate list
		list.push_back(1);
		list.push_back(2);
		list.push_back(3);

		// Check normal removal
		assert_eq!(list.pop_back(), Some(3));
		assert_eq!(list.pop_back(), Some(2));

		// Push some more just to make sure nothing's corrupted
		list.push_back(4);
		list.push_back(5);

		// Check normal removal
		assert_eq!(list.pop_back(), Some(5));
		assert_eq!(list.pop_back(), Some(4));

		// Check exhaustion
		assert_eq!(list.pop_back(), Some(1));
		assert_eq!(list.pop_back(), None);
	}

	#[test]
	fn peek() {
		let mut list = List::new();
		assert!(list.peek_front().is_none());
		assert!(list.peek_back().is_none());
		assert!(list.peek_front_mut().is_none());
		assert!(list.peek_back_mut().is_none());

		list.push_front(1);
		list.push_front(2);
		list.push_front(3);

		assert_eq!(list.peek_front().unwrap(), &3);
		assert_eq!(&mut *list.peek_front_mut().unwrap(), &mut 3);
		assert_eq!(list.peek_back().unwrap(), &1);
		assert_eq!(&mut *list.peek_back_mut().unwrap(), &mut 1);
	}

	#[test]
	fn into_iter() {
		let mut list = List::new();
		list.push_front(1);
		list.push_front(2);
		list.push_front(3);

		let mut iter = list.into_iter();
		assert_eq!(iter.next(), Some(3));
		assert_eq!(iter.next_back(), Some(1));
		assert_eq!(iter.next(), Some(2));
		assert_eq!(iter.next_back(), None);
		assert_eq!(iter.next(), None);
	}

	#[test]
	fn stress_test() {
		let mut list = List::new();

		// Test alternating operations: push_front then pop_front, then push_back then
		// pop_back
		for i in 0..1e6 as u64 {
			list.push_front(i);
			assert_eq!(list.pop_front(), Some(i));
			list.push_back(i);
			assert_eq!(list.pop_back(), Some(i));
		}
		assert_eq!(list.pop_front(), None);
		assert_eq!(list.pop_back(), None);
	}

	#[test]
	fn no_memory_leak() {
		let mut list = List::new();
		let item = Rc::new(());

		// Add multiple references
		for _ in 0..1000 {
			list.push_front(Rc::clone(&item));
			list.push_back(Rc::clone(&item));
		}

		// Check reference count
		// original reference + 2000 clones
		assert_eq!(Rc::strong_count(&item), 2001);

		// Clear the list
		while list.pop_front().is_some() {}

		// All nodes should be released, only the original reference remains
		assert_eq!(Rc::strong_count(&item), 1);
	}
}
