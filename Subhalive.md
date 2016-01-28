(Move this to Halive repo)

Rumpus is a model project for SubHalive and exposes one subtlety of its use:

Say we've got some code in src/ that is used by both the executable and the
plugins. The wrong thing to do is to pass "src" to startGHC (aka subhalive)
such that src/ is added to subhalive's search path. This is wrong 
because the executable is compiled code, whereas subhalive is interpreted,
thus if subhalive returns values inhabiting datatypes defined in src,
it will be returning the interpreted versions to compiled code 
expecting compiled versions. This results in a segfault.

Instead, you must split the code into an executable, a library, and the plugins.
The executable will simply be "import qualified Lib; main = Lib.main". 
For the plugins, pass no include directories to startGHC,
(except if they are for other interpreted code used only by the plugins),
instead relying on the package-finding functionality of subhalive to find Lib. 
Now both Subhalive and the executable are relying on the compiled version 
of Lib, and everything will work wonderfully!
