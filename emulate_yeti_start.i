// Only prepare these autoloads if yeti is not in the path.
// Yeti's autoload should also cancel this by calling
// autoload, "emulate_yeti.i";
if (!find_in_path("yeti.i", takefirst=1)) {
  autoload, "emulate_yeti.i";
  autoload, "emulate_yeti.i", grow_dimlist, unref, swap;
  autoload, "emulate_yeti.i", is_matrix, is_integer, is_real, is_complex;
  autoload, "emulate_yeti.i", is_numerical, is_string, is_integer_scalar;
  autoload, "emulate_yeti.i", round, strlower, strupper;
 }
