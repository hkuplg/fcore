package f2j;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.*;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;


public class TypeServer {

  private final static Map<Class<?>, Class<?>> map = new HashMap<Class<?>, Class<?>>();
  private final static Map<String, Class<?>> arrayMap = new HashMap<String, Class<?>>();

  static {
    map.put(boolean.class, Boolean.class);
    map.put(byte.class, Byte.class);
    map.put(short.class, Short.class);
    map.put(char.class, Character.class);
    map.put(int.class, Integer.class);
    map.put(long.class, Long.class);
    map.put(float.class, Float.class);
    map.put(double.class, Double.class);
    map.put(void.class, Void.class);
  }

  static {
    arrayMap.put("char[]", char[].class);
    arrayMap.put("int[]", int[].class);
    arrayMap.put("byte[]", byte[].class);
    arrayMap.put("boolean[]", boolean[].class);
    arrayMap.put("long[]", long[].class);
    arrayMap.put("double[]", double[].class);
  }

  public static void main(String[] args) {
    Scanner inp = new Scanner(System.in);
    while (true) {
      if (!inp.hasNextLine()) continue;
      String line = inp.nextLine();
      String[] words = line.split(" ");
      Object result = false;
      switch (words[0]) {
      case "qType":
        result = queryType(words[1]);
        break;
      case "qConstructor":
        result = queryNew(words[1], Arrays.copyOfRange(words, 2, words.length));
        break;
      case "qMethod":
        result = queryMethod(words[1], words[2], Arrays.copyOfRange(words, 3, words.length), false);
        break;
      case "qStaticMethod":
        result = queryMethod(words[1], words[2], Arrays.copyOfRange(words, 3, words.length), true);
        break;
      case "qField":
        result = queryField(words[1], words[2], false);
        break;
      case "qStaticField":
        result = queryField(words[1], words[2], true);
        break;
      default:
        System.err.println("######## BAD QUERY ########");
        break;
      }
      System.out.println(result.toString());
    }
  }

  public static boolean queryType(String classFullName) {
    try {
      Class.forName(classFullName);
    } catch (ClassNotFoundException e) {
      return false;
    }
    return true;
  }


  public static boolean queryNew(String classFullName, String... constructorArgs) {
    try {
      Class<?> c = Class.forName(classFullName);
      Constructor<?>[] cList = c.getConstructors();
      Class<?>[] givenClasses = getClassArray(constructorArgs);

      for (Constructor cs : cList) {
        Class<?>[] actualClasses = cs.getParameterTypes();
        if (isArrayAssignableFrom(actualClasses, givenClasses))
          return true;
      }
      return false;
    } catch (Exception e) {
      return false;
    }
  }


  public static String queryMethod(String classFullName, final String methodName, String[] methodArgs, final boolean stat) {
    try {
      Class<?> c = Class.forName(classFullName);
      final Class<?>[] givenClasses = getClassArray(methodArgs);
      Method[] mList = c.getMethods();

      List<Object> ms = Arrays.stream(mList).filter(new Predicate<Method>() {
          @Override
          public boolean test(Method m) {
            Class<?>[] actualClasses = m.getParameterTypes();
            return Modifier.isStatic(m.getModifiers()) == stat
            && m.getName().equals(methodName)
            && isArrayAssignableFrom(actualClasses, givenClasses);
          }
        }).map(new Function<Method, Class<?>>() {
            @Override
            public Class<?> apply(Method m) {
              return m.getReturnType();
            }
          }).sorted(new Comparator<Class<?>>() {
              @Override
              public int compare(Class<?> m1, Class<?> m2) {
                return m1.isAssignableFrom(m2) ? 1 : -1;
              }
            }).collect(Collectors.toList());

      if (ms.size() == 0) return "$";
      else return classFix((Class<?>) ms.get(0)).getCanonicalName();
    } catch (Exception e) {
      return "$";
    }
  }


  public static String queryField(String classFullName, String fieldName, boolean stat) {
    try {
      Class<?> c = Class.forName(classFullName);
      Field f = c.getField(fieldName);
      if (Modifier.isStatic(f.getModifiers()) == stat)
        return classFix(f.getType()).getCanonicalName();
      else
        return "$";
    } catch (Exception e) {
      return "$";
    }
  }



  private static Class<?>[] getClassArray(String[] names) throws ClassNotFoundException {

    Class<?>[] classes = new Class<?>[names.length];
    for (int i = 0; i < names.length; i++) {
      classes[i] = arrayMap.containsKey(names[i]) ? arrayMap.get(names[i]) : Class.forName(names[i]);
    }
    return classes;
  }

  // a -> super; b -> sub
  private static boolean isArrayAssignableFrom(Class<?>[] a, Class<?>[] b) {
    if (a.length != b.length) return false;
    for (int i = 0; i < a.length; i++)
      if (!classFix(a[i]).isAssignableFrom(classFix(b[i])))
        return false;
    return true;
  }

  // int -> Integer
  private static Class<?> classFix(Class<?> c) {
    return c.isPrimitive() ? map.get(c) : c;
  }

}
