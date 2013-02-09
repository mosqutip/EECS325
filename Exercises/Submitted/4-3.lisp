#|
A 3tree is a very basic tree structure with a data pointer,
and (up to) three pointers to children.
|#
(defstruct 3tree
  (data nil)
  (left nil)
  (middle nil)
  (right nil))

#|
3tree-clone copies a 3tree structure by copying the root node
data, then recursively copying each of the children.
|#
(defun 3tree-clone (3tr)
  (if (null 3tr)
      nil
    (make-3tree :data (3tree-data 3tr)
                :left (3tree-clone (3tree-left 3tr))
                :middle (3tree-clone (3tree-middle 3tr))
                :right (3tree-clone (3tree-right 3tr)))))

#|
3tree-member compares a data object to the current
node, then recursively compares against each child
node, returning true if there is a match in the tree.
|#
(defun 3tree-member (x 3tr)
  (if (null 3tr)
      nil
    (or (eql x (3tree-data 3tr))
        (3tree-member x (3tree-left 3tr))
        (3tree-member x (3tree-middle 3tr))
        (3tree-member x (3tree-right 3tr)))))