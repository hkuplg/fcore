package f2j;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

@Retention(RetentionPolicy.RUNTIME)
public @interface ModuleFunction {
  String name();
  String gname();
  String signature();
}
