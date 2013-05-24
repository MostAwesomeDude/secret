* Builtins removed

  * ``hasattr()``
  * ``isinstance()``

* Syntax removed

  * Some argument and parameter syntax removed

    * Packed keyword arguments

  * Inheritance: Inheritance does not make syntactic sense without semantics powering it.
  * Print statement
  * Semicolons
  * Tab characters: Deal with it. We don't do vertical tabs or backspaces
    either.

* Semantics removed

  * Augmented assignment: Augmented assignments now always desugar to standard assignment; `expr += value` becomes `expr = expr + value`.
  * Inheritance: Creating objects which are of the same nature as other objects is no longer possible nor especially meaningful. Compose objects instead.
