; Modelling a bank

( defclass bank ()
  (
    ( missionaries :accessor bank-missionaries :initarg :missionaries )
    ( cannibals :accessor bank-cannibals :initarg :cannibals )
    ( boat :accessor bank-boat :initarg :boat )
  )
)

( defmethod display ( ( b bank ) )
  ( format t "MISSIONARIES=~A CANNIBALS=~A BOAT=~A~%" ( bank-missionaries b ) ( bank-cannibals b ) ( bank-boat b ) )
  nil
)

( defmethod equal-bank-p ( (b1 bank) (b2 bank) )
  ( and 
    ( equal ( bank-missionaries b1 ) ( bank-missionaries b2 ) ) 
    ( equal ( bank-cannibals b1 ) ( bank-cannibals b2 ) ) 
    ( equal ( bank-boat b1 ) ( bank-boat b2 ) )
  )
)


;------------------------------------------------------------------
; Modelling a state

( defclass state ()
  (
    ( left-bank :accessor state-left-bank :initarg :left-bank )
    ( right-bank :accessor state-right-bank :initarg :right-bank )
  )
)

( defmethod display ( ( s state ) )
  ( display ( state-left-bank s ) )
  ( display ( state-right-bank s ) )
  nil
)

( defmethod copy-state ( ( s state ) )
  ( make-instance 'state 
    :left-bank ( state-left-bank s)
    :right-bank ( state-right-bank s)
  )
)

( defmethod equal-state-p ( (s1 state) ( s2 state ) )
  ( and
    ( equal-bank-p ( state-left-bank s1 ) ( state-left-bank s2 ) )
    ( equal-bank-p ( state-right-bank s1 ) ( state-right-bank s2 ) )
  )
)


;------------------------------------------------------------------
; Modelling a node

( defclass node ()
  (
    ( name :accessor node-name :initarg :name )
    ( state :accessor node-state :initarg :state )
    ( parent :accessor node-parent :initarg :parent )
    ( operator :accessor node-operator :initarg :operator )
  )
)

( defmethod display ( ( n node ) )
  ( princ ( node-name n ) )
  ( if ( not ( rootp n ) )
    ( let ()
      ( princ " " ) ( princ ( node-name ( node-parent n ) ) ) ( princ " " )
      ( display ( node-operator n ) )
    )
  )
  ( terpri )
  ( display ( node-state n ) )
  nil
)

( defmethod rootp ( (n node) )
  ( equal ( node-name n ) "ROOT" )
)


;------------------------------------------------------------------
; Modelling an operator

( defclass operator ()
  (
    ( name :accessor operator-name :initarg :name )
    ( precondition :accessor operator-precondition :initarg :precondition )
    ( description :accessor operator-description :initarg :description )
  )
)

( defmethod display ( ( o operator ) )
  ( format t "~A " ( operator-name o ) )
)


;------------------------------------------------------------------
; Modelling a name-generator
 
( defclass name-generator ()
  ( ( prefix :accessor name-generator-prefix :initarg :prefix :initform "name" )
    ( nr :accessor name-generator-nr :initform 0 )
  )
)

( defmethod next ( ( ng name-generator ) )
  ( setf ( name-generator-nr ng ) ( + 1 ( name-generator-nr ng ) ) )
  ( concatenate 'string
    ( name-generator-prefix ng )
    ( write-to-string ( name-generator-nr ng ) )
  )
)


;------------------------------------------------------------------
; Main method

( defmethod mc ()
  ( establish-operators )
  ( setup )
  ( setf *trace-search* t )
  ( solve )
)

;------------------------------------------------------------------
; establish-operators

( defmethod establish-operators ()
  ( setf *move-m-lr* ( make-instance 'operator
                       :name 'move-m-lr
                       :precondition "There is at least 1 missionary on the left bank."
                       :description "1 missionary goes from the left bank to the right bank."
                      )
  )
  ( setf *move-m-rl* ( make-instance 'operator
                       :name 'move-m-rl
                       :precondition "There is at least 1 missionary on the right bank."
                       :description "1 missionary goes from the right bank to the left bank."
                     )
  )
  ( setf *move-c-lr* ( make-instance 'operator
                       :name 'move-c-lr
                       :precondition "There is at least 1 cannibal on the left bank."
                       :description "1 cannibal goes from the left bank to the right bank."
                      )
  )
  ( setf *move-c-rl* ( make-instance 'operator
                       :name 'move-c-rl
                       :precondition "There is at least 1 cannibal on the right bank."
                       :description "1 cannibal goes from the right bank to the left bank."
                     )
  )
  ( setf *move-mm-lr* ( make-instance 'operator
                        :name 'move-mm-lr
                        :precondition "There are at least 2 missionaries on the left bank."
                        :description "2 missionaries go from the left bank to the right bank."
                      )
  )
  ( setf *move-mm-rl* ( make-instance 'operator
                        :name 'move-mm-rl
                        :precondition "There are at least 2 missionaries on the right bank."
                        :description "2 missionaries go from the right bank to the left bank."
                      )
  )
  ( setf *move-cc-lr* ( make-instance 'operator
                        :name 'move-cc-lr
                        :precondition "There are at least 2 cannibals on the left bank."
                        :description "2 cannibals go from the left bank to the right bank."
                      )
  )
  ( setf *move-cc-rl* ( make-instance 'operator
                        :name 'move-cc-rl
                        :precondition "There are at least 2 cannibals on the right bank."
                        :description "2 cannibals go from the right bank to the left bank."
                      )
  )
  ( setf *move-cm-lr* ( make-instance 'operator
                        :name 'move-cm-lr
                        :precondition "There is at least 1 missionary and 1 cannibal on the left bank."
                        :description "1 missionary and 1 cannibal go from the left bank to the right bank."
                      )
  )
  ( setf *move-cm-rl* ( make-instance 'operator
                        :name 'move-cm-rl
                        :precondition "There is at least 1 missionary and 1 cannibal on the right bank."
                        :description "1 missionary and 1 cannibal go from the right bank to the left bank."
                      )
  )
)

( defmethod move-m-lr ( ( o operator ) ( s state ) &aux left-bank right-bank new-state )
  ( setf left-bank ( make-instance 'bank
                     :missionaries ( remove-one 'm ( bank-missionaries ( state-left-bank s ) ) ) 
                     :cannibals ( bank-cannibals ( state-left-bank s ) )  
                     :boat 'nil
                   )
  )
  ( setf right-bank ( make-instance 'bank
                     :missionaries ( append '(m) ( bank-missionaries ( state-right-bank s ) ) )
                     :cannibals ( bank-cannibals ( state-right-bank s ) )  
                     :boat 'b
                    )
  )
  ( setf new-state ( make-instance 'state
                     :right-bank right-bank
                     :left-bank left-bank
                   )
  )
  new-state
)

( defmethod move-m-rl ( ( o operator ) ( s state ) &aux left-bank right-bank new-state )
  ( setf left-bank ( make-instance 'bank
                     :missionaries ( append '(m) ( bank-missionaries ( state-left-bank s ) ) )
                     :cannibals ( bank-cannibals ( state-left-bank s ) )  
                     :boat 'b
                   )
  )
  ( setf right-bank ( make-instance 'bank
                     :missionaries ( remove-one 'm ( bank-missionaries ( state-right-bank s ) ) ) 
                     :cannibals ( bank-cannibals ( state-right-bank s ) )  
                     :boat 'nil
                    )
  )
  ( setf new-state ( make-instance 'state
                     :right-bank right-bank
                     :left-bank left-bank
                   )
  )
  new-state
)

( defmethod move-c-lr ( ( o operator ) ( s state ) &aux left-bank right-bank new-state )
  ( setf left-bank ( make-instance 'bank
                     :missionaries ( bank-missionaries ( state-left-bank s ) ) 
                     :cannibals ( remove-one 'c ( bank-cannibals ( state-left-bank s ) ) ) 
                     :boat 'nil
                   )
  )
  ( setf right-bank ( make-instance 'bank
                     :missionaries ( bank-missionaries ( state-right-bank s ) )
                     :cannibals ( append '(c) ( bank-cannibals ( state-right-bank s ) ) )
                     :boat 'b
                    )
  )
  ( setf new-state ( make-instance 'state
                     :right-bank right-bank
                     :left-bank left-bank
                   )
  )
  new-state
)

( defmethod move-c-rl ( ( o operator ) ( s state ) &aux left-bank right-bank new-state )
  ( setf left-bank ( make-instance 'bank
                     :missionaries ( bank-missionaries ( state-left-bank s ) )
                     :cannibals ( append '(c) ( bank-cannibals ( state-left-bank s ) ) ) 
                     :boat 'b
                   )
  )
  ( setf right-bank ( make-instance 'bank
                     :missionaries ( bank-missionaries ( state-right-bank s ) ) 
                     :cannibals ( remove-one 'c ( bank-cannibals ( state-right-bank s ) ) )   
                     :boat 'nil
                    )
  )
  ( setf new-state ( make-instance 'state
                     :right-bank right-bank
                     :left-bank left-bank
                   )
  )
  new-state
)

( defmethod move-mm-lr ( ( o operator ) ( s state ) &aux left-bank right-bank new-state )
  ( setf left-bank ( make-instance 'bank
                     :missionaries ( remove-two 'm ( bank-missionaries ( state-left-bank s ) ) ) 
                     :cannibals ( bank-cannibals ( state-left-bank s ) )  
                     :boat 'nil
                   )
  )
  ( setf right-bank ( make-instance 'bank
                     :missionaries ( append '(m m) ( bank-missionaries ( state-right-bank s ) ) )
                     :cannibals ( bank-cannibals ( state-right-bank s ) )  
                     :boat 'b
                    )
  )
  ( setf new-state ( make-instance 'state
                     :right-bank right-bank
                     :left-bank left-bank
                   )
  )
  new-state
)

( defmethod move-mm-rl ( ( o operator ) ( s state ) &aux left-bank right-bank new-state )
  ( setf left-bank ( make-instance 'bank
                     :missionaries ( append '(m m) ( bank-missionaries ( state-left-bank s ) ) )
                     :cannibals ( bank-cannibals ( state-left-bank s ) )  
                     :boat 'b
                   )
  )
  ( setf right-bank ( make-instance 'bank
                     :missionaries ( remove-two 'm ( bank-missionaries ( state-right-bank s ) ) ) 
                     :cannibals ( bank-cannibals ( state-right-bank s ) )  
                     :boat 'nil
                    )
  )
  ( setf new-state ( make-instance 'state
                     :right-bank right-bank
                     :left-bank left-bank
                   )
  )
  new-state
)

( defmethod move-cc-lr ( ( o operator ) ( s state ) &aux left-bank right-bank new-state )
  ( setf left-bank ( make-instance 'bank
                     :missionaries ( bank-missionaries ( state-left-bank s ) )  
                     :cannibals ( remove-two 'c ( bank-cannibals ( state-left-bank s ) ) ) 
                     :boat 'nil
                   )
  )
  ( setf right-bank ( make-instance 'bank
                     :missionaries ( bank-missionaries ( state-right-bank s ) )  
                     :cannibals  ( append '(c c) ( bank-cannibals ( state-right-bank s ) ) )
                     :boat 'b
                    )
  )
  ( setf new-state ( make-instance 'state
                     :right-bank right-bank
                     :left-bank left-bank
                   )
  )
  new-state
)

( defmethod move-cc-rl ( ( o operator ) ( s state ) &aux left-bank right-bank new-state )
  ( setf left-bank ( make-instance 'bank
                     :missionaries ( bank-missionaries ( state-left-bank s ) )  
                     :cannibals ( append '(c c) ( bank-cannibals ( state-left-bank s ) ) )
                     :boat 'b
                   )
  )
  ( setf right-bank ( make-instance 'bank
                     :missionaries ( bank-missionaries ( state-right-bank s ) ) 
                     :cannibals ( remove-two 'c ( bank-cannibals ( state-right-bank s ) ) ) 
                     :boat nil
                    )
  )
  ( setf new-state ( make-instance 'state
                     :right-bank right-bank
                     :left-bank left-bank
                   )
  )
  new-state
)

( defmethod move-cm-lr ( ( o operator ) ( s state ) &aux left-bank right-bank new-state )
  ( setf left-bank ( make-instance 'bank
                     :missionaries ( remove-one 'm ( bank-missionaries ( state-left-bank s ) ) ) 
                     :cannibals ( remove-one 'c ( bank-cannibals ( state-left-bank s ) ) ) 
                     :boat 'nil
                   )
  )
  ( setf right-bank ( make-instance 'bank
                     :missionaries ( append '(m) ( bank-missionaries ( state-right-bank s ) ) )
                     :cannibals ( append '(c) ( bank-cannibals ( state-right-bank s ) ) )
                     :boat 'b
                    )
  )
  ( setf new-state ( make-instance 'state
                     :right-bank right-bank
                     :left-bank left-bank
                   )
  )
  new-state
)

( defmethod move-cm-rl ( ( o operator ) ( s state ) &aux left-bank right-bank new-state )
  ( setf left-bank ( make-instance 'bank
                     :missionaries ( append '(m) ( bank-missionaries ( state-left-bank s ) ) )
                     :cannibals ( append '(c) ( bank-cannibals ( state-left-bank s ) ) ) 
                     :boat 'b
                   )
  )
  ( setf right-bank ( make-instance 'bank
                     :missionaries ( remove-one 'm ( bank-missionaries ( state-right-bank s ) ) ) 
                     :cannibals ( remove-one 'c ( bank-cannibals ( state-right-bank s ) ) ) 
                     :boat 'nil
                    )
  )
  ( setf new-state ( make-instance 'state
                     :right-bank right-bank
                     :left-bank left-bank
                   )
  )
  new-state
)


;------------------------------------------------------------------
; The setup

( defmethod setup ( &aux root lb rb istate )
  ;; establish root node
  ( setf lb ( make-instance 'bank :missionaries '(m m m) :cannibals '(c c c) :boat 'b ) )
  ( setf rb ( make-instance 'bank :missionaries '() :cannibals '() :boat nil ) )
  ( setf istate ( make-instance 'state :left-bank lb :right-bank rb ) )
  ( setf root ( make-instance 'node :state istate :name "ROOT" ) )
  ;; initialize list of unexplored nodes 
  ( setf *unexplored* ( list root ) )
  ;; initialize list of explored nodes
  ( setf *explored* () )
  ;; get ready to create good names
  ( setf *ng* ( make-instance 'name-generator :prefix "N" ) )
)


;------------------------------------------------------------------
; breadth first search
 
( defmethod solve ( &aux kids e-node )
  ( if *trace-search*
    ( let ()
      ( terpri ) ( write-line ">>> Solve" ) ( terpri )
      ( display-explored-nodes )
      ( display-unexplored-nodes )
    )
  )
  ( cond 
    ( ( null *unexplored* )
      ( write-line "There is no solution." )
      ( return-from solve NIL )
    )
  )
  ( setf e-node ( pop *unexplored* ) )
  ( if *trace-search*
    ( let () 
      ( display-e-node e-node )
    )
  )
  ( cond
    ( ( goalp ( node-state e-node ) )
      ( display-solution e-node )
    )
    ( ( exploredp e-node ) 
      ( solve )
    )
    ( t
      ( push e-node *explored* )
      ( setf kids ( children-of e-node ) )
      ( setf *unexplored* ( append *unexplored* kids ) )
      ( solve )
    )
  )
  NIL
)

( defmethod display-explored-nodes ()
  ( terpri ) ( prin1 'EXPLORED ) ( terpri )
  ( mapcar #'display *explored* )
  ( terpri )
  nil
)

( defmethod display-unexplored-nodes ()
  ( terpri ) ( prin1 'UNEXPLORED ) ( terpri )
  ( mapcar #'display *unexplored* )
  ( terpri )
  nil
)

( defmethod display-e-node ( ( n node ) )
  ( terpri ) ( princ "E-NODE" ) ( terpri )
  ( display n )
)

( defmethod goalp ( ( s state ) )
    ( and ( equal ( bank-missionaries ( state-right-bank s ) ) '(m m m) )
          ( equal ( bank-cannibals ( state-right-bank s ) ) '(c c c) ) 
    )
)

( defmethod display-solution ( ( n node ) )
  ( cond
    ( ( rootp n ) 
      ( terpri ) 
    )
    ( t 
      ( display-solution ( node-parent n ) )
      ( princ ( operator-description ( node-operator n ) ) )
      ( terpri ) 
    ) 
  )
  nil
)

( defmethod exploredp ( ( n node ) )
  ( member-state-p ( node-state n ) *explored* )
)


;-------------------------------------------------------------------
; generating children

( defmethod children-of ( (e-node node) &aux kids )
  ( if ( feastp ( node-state e-node ) )
    ( return-from children-of nil ) 
  )
  ( if ( applicablep *move-mm-lr* e-node ) 
    ( push ( child-of e-node *move-mm-lr* ) kids )
  )
  ( if ( applicablep *move-mm-rl* e-node ) 
    ( push ( child-of e-node *move-mm-rl* ) kids )
  )
  ( if ( applicablep *move-m-lr* e-node ) 
    ( push ( child-of e-node *move-m-lr*  ) kids )
  )
  ( if ( applicablep *move-m-rl* e-node ) 
    ( push ( child-of e-node *move-m-rl*  ) kids )
  )
  ( if ( applicablep *move-cc-lr* e-node )  
    ( push ( child-of e-node *move-cc-lr* ) kids )
  )
  ( if ( applicablep *move-cc-rl* e-node )  
    ( push ( child-of e-node *move-cc-rl* ) kids )
  )
  ( if ( applicablep *move-c-lr* e-node )  
    ( push ( child-of e-node *move-c-lr*  ) kids )
  )
  ( if ( applicablep *move-c-rl* e-node )  
    ( push ( child-of e-node *move-c-rl*  ) kids )
  )
  ( if ( applicablep *move-cm-lr* e-node )  
    ( push ( child-of e-node *move-cm-lr* ) kids )
  )
  ( if ( applicablep *move-cm-rl* e-node )  
    ( push ( child-of e-node *move-cm-rl* ) kids )
  )
  kids
)

( defmethod feastp ( ( s state ) &aux left-bank right-bank )
  ( setf left-bank ( state-left-bank s ) )
  ( setf right-bank ( state-right-bank s ) )
  ( if ( or
         ( and
           ( not ( eq nil ( bank-missionaries left-bank ) ) )
           ( > ( length ( bank-cannibals left-bank ) ) ( length ( bank-missionaries left-bank ) ) ) 
         )
         ( and
           ( not ( eq nil ( bank-missionaries right-bank ) ) )
           ( > ( length ( bank-cannibals right-bank ) ) ( length ( bank-missionaries right-bank ) ) ) 
         )
       )
       t
  nil
  )
)

( defmethod child-of ( ( n node ) ( o operator ) &aux c )
  ( setf new-node ( make-instance 'node ) )
  ( setf ( node-name new-node ) ( next *ng* ) )
  ( setf ( node-parent new-node ) n )
  ( setf ( node-operator new-node ) o )
  ( setf c ( copy-state ( node-state n ) ) )
  ( setf c ( apply-operator o c ) )
  ( setf ( node-state new-node ) c )
  new-node
)

( defmethod apply-operator ( ( o operator ) ( s state ) )
  ( funcall ( operator-name o ) o s )
)


;------------------------------------------------------------------
; Additional methods

( defmethod remove-one ( l lst )
  ( cond
    ( ( null lst ) () )
    ( ( equal l ( car lst ) ) ( cdr lst ) )
    ( t ( cons ( car lst ) ( remove-one l ( cdr lst ) ) ) )
  )
)  

( defmethod remove-two ( l lst )
  ( remove-one l ( remove-one l lst ) )
) 

( defmethod boatp ( ( b bank ) )
  ( eq ( bank-boat b ) 'b )
)

( defmethod member-state-p ( ( s state ) ( l list ) )
  ( cond
    ( ( null l )
       nil
    )
    ( ( equal-state-p s ( node-state ( car l ) ) )
      t
    )
    ( t
      ( member-state-p s ( cdr l ) )
    )
  )
)

( defmethod applicablep ( ( o operator ) ( n node ) &aux left-bank right-bank s )
  ( setf left-bank ( state-left-bank ( node-state n ) ) )
  ( setf right-bank ( state-right-bank ( node-state n ) ) )
  ( setf s ( node-state n ) )
  ( if ( boatp left-bank )
    ( cond
      ( ( equal ( operator-name o ) 'move-m-lr ) ( applicable-move-m-lr left-bank s n ) )
      ( ( equal ( operator-name o ) 'move-c-lr ) ( applicable-move-c-lr left-bank s n ) )
      ( ( equal ( operator-name o ) 'move-mm-lr ) ( applicable-move-mm-lr left-bank s n ) )
      ( ( equal ( operator-name o ) 'move-cc-lr ) ( applicable-move-cc-lr left-bank s n ) )
      ( ( equal ( operator-name o ) 'move-cm-lr ) ( applicable-move-cm-lr left-bank s n ) )
    )
    ( cond
      ( ( equal ( operator-name o ) 'move-m-rl ) ( applicable-move-m-rl right-bank s n ) )
      ( ( equal ( operator-name o ) 'move-c-rl ) ( applicable-move-c-rl right-bank s n ) )
      ( ( equal ( operator-name o ) 'move-mm-rl ) ( applicable-move-mm-rl right-bank s n ) )
      ( ( equal ( operator-name o ) 'move-cc-rl ) ( applicable-move-cc-rl right-bank s n ) )
      ( ( equal ( operator-name o ) 'move-cm-rl ) ( applicable-move-cm-rl right-bank s n ) )
    )
  )
)

( defmethod applicable-move-m-lr ( ( left-bank bank ) ( s state ) ( n node ) ) 
  ( >= ( length ( bank-missionaries left-bank ) ) 1 )
)

( defmethod applicable-move-m-rl ( ( right-bank bank ) ( s state ) ( n node ) )
  ( >= ( length ( bank-missionaries right-bank ) ) 1 )
)

( defmethod applicable-move-c-lr ( ( left-bank bank ) ( s state ) ( n node ) )
  ( >= ( length ( bank-cannibals left-bank ) ) 1 )
)

( defmethod applicable-move-c-rl ( ( right-bank bank ) ( s state ) ( n node ) )
  ( >= ( length ( bank-cannibals right-bank ) ) 1 )
)

( defmethod applicable-move-mm-lr ( ( left-bank bank ) ( s state ) ( n node ) )
  ( > ( length ( bank-missionaries left-bank ) ) 1 )
)

( defmethod applicable-move-mm-rl ( ( right-bank bank ) ( s state ) ( n node ) )
  ( > ( length ( bank-missionaries right-bank ) ) 1 )
)

( defmethod applicable-move-cc-lr ( ( left-bank bank ) ( s state ) ( n node ) )
  ( > ( length ( bank-cannibals left-bank ) ) 1 )
)

( defmethod applicable-move-cc-rl ( ( right-bank bank ) ( s state ) ( n node ) )
  ( > ( length ( bank-cannibals right-bank ) ) 1 )
)

( defmethod applicable-move-cm-lr ( ( left-bank bank ) ( s state ) ( n node ) )
  ( and 
    ( >= ( length ( bank-cannibals left-bank ) ) 1 )
    ( >= ( length ( bank-missionaries left-bank ) ) 1 )
  )
)

( defmethod applicable-move-cm-rl ( ( right-bank bank ) ( s state ) ( n node ) )
  ( and 
    ( >= ( length ( bank-cannibals right-bank ) ) 1 )
    ( >= ( length ( bank-missionaries right-bank ) ) 1 )
  )
)
