class Ball
{};

int main()
{

  // new (placement_params) (type) [initializer]
  new (1) (int) (1);
  new (1) (int) {1};
  new (1) (int);

  // new (placement_params) type [initializer]
  new (1) int (1);
  new (1) int {1};
  new (1) int;

  // new (type)
  new (int);

  // new (type) initializer
  new (int) (1);
  new (int) {1};

  // new type
  new int;

  // new type [initializer]
  new int (1);
  new int {1};
}
