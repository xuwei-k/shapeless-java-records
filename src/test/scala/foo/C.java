package foo;

import java.util.List;

public record C<D>(D x1, List<D> x2, C<String> x3) implements Base {
}
