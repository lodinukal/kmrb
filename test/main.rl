package oxy;

urg: u32 = 0

@ecs.Component
Health :: struct {
}

main :: proc() {
  system := ## ecs.new_system(Health);
  defer ecs.destroy(&system);
  for x in 0..10 {
      print("hello world\n")
  }
  #assert(1 == 1)
}

// package oxy;
// urg: u32 = 0
// @ecs.Component
// struct Health:
//     pass
//
// union SystemArgs:
//     Health: Health
//     Position: Position
//
// fun main():
//     system = ecs.new_system(Health);
//     for x in range(10):
//         print("hello world\n")
//     assert 1 == 1
//     ecs.destroy(system)
//     return 0