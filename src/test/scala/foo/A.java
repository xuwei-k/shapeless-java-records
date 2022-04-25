package foo;

import java.util.List;

public record A<X2>(int x1, X2 x2, String y, List<String> z) implements Base<X2> {
  public int x1(int a) {
    return a;
  }
}
