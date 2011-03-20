------------------------------------------------------------------------------
--- Library for handling global entities.
--- A global entity has a name declared in the program.
--- Its value can be accessed and modified by IO actions.
--- Furthermore, global entities can be declared as persistent so that
--- their values are stored across different program executions.
---
--- Currently, it is still experimental so that its interface might
--- be slightly changed in the future.
---
--- A global entity <code>g</code> with an initial value <code>v</code>
--- of type <code>t</code> must be declared by:
--- 
--- <code>g :: Global t</code><br/>
--- <code>g = global v spec</code>
---
--- Here, the type <code>t</code> must not contain type variables and
--- <code>spec</code> specifies the storage mechanism for the
--- global entity (see type <code>GlobalSpec</code>).
---
---
--- @author Michael Hanus
--- @version June 2007
------------------------------------------------------------------------------

module Global(Global,GlobalSpec(..),global,readGlobal,writeGlobal) where

----------------------------------------------------------------------

--- The abstract type of a global entity.
data Global _ -- a = GlobalDef a GlobalSpec

--- <code>global</code> is only used for the declaration of a global value
--- and should not be used elsewhere. In the future, it might become a keyword.
global :: a -> GlobalSpec -> Global a
global external --v s = GlobalDef v s

--- The storage mechanism for the global entity.
--- @cons Temporary - the global value exists only during a single execution
---                   of a program
--- @cons Persistent f - the global value is stored persisently in file f
---                     (which is created and initialized if it does not exists)
data GlobalSpec = Temporary  | Persistent String


--- Reads the current value of a global.
readGlobal :: Global a -> IO a
readGlobal g = prim_readGlobal $# g

prim_readGlobal :: Global a -> IO a
prim_readGlobal external


--- Updates the value of a global.
--- The value is evaluated to a ground constructor term before it is updated.
writeGlobal :: Global a -> a -> IO ()
writeGlobal g v = (prim_writeGlobal $# g) $## v

prim_writeGlobal :: Global a -> a -> IO ()
prim_writeGlobal external


------------------------------------------------------------------------
