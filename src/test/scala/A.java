package foo;

import java.util.List;

public record A(int x, String y, List<String> z) implements Base {
  public int x(int a) {
    return a;
  }
}
