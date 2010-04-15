// Only prepare these autoloads if yeti is not in the path.
// Yeti's autoload should also cancel this by calling
// autoload, "emulate_yeti.i";
if (!find_in_path("yeti.i", takefirst=1)) {
  autoload, "emulate_yeti.i";
  autoload, "emulate_yeti.i", grow_dimlist, is_integer_scalar;
  autoload, "emulate_yeti.i", strlower, strupper;
 }

// the following are yorick buildins since 2.1.05x (april 10,2010)
if ( (typeof(swap)!="builtin") && (!find_in_path("yeti.i", takefirst=1)) ) {
  // we're working with yorick pre-apr2010, and
  // yeti is not in the path. Let's autoload these functions:
  autoload, "emulate_yeti.i", unref, swap;
  autoload, "emulate_yeti.i", is_matrix, is_integer, is_real, is_complex;
  autoload, "emulate_yeti.i", is_numerical, is_string, round;
 }
