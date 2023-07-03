unit RALTypes;

interface

// compatibility types
{
  These definitions are meant to keep same code across all versions of the IDE
  or IDEs that might differ on the charset code or basic type length.
  Expect heavy usage of IFDEFs at this point
}

type
  IntegerRAL = integer;
  StringRAL = string;
  Int64RAL = int64;
  DoubleRAL = double;
  CharRAL = char;

implementation

end.
