package foo;

import java.util.List;

public record A(int x, String y) implements Base {
  public int x(int a) {
    return a;
  }
}
