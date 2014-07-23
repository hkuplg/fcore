import java.util.Arrays;


public class TypeServer {

    public static void main(String[] args) {
        if (args[0].equals("queryType")) {
            System.out.println(queryType(args[1]));
        } else if (args[0].equals("queryNew")) {
            System.out.println(queryNew(args[1], Arrays.copyOfRange(args, 2, args.length)));
        } else if (args[0].equals("queryMethod")) {
            System.out.println(queryMethod(args[1], args[2], Arrays.copyOfRange(args, 3, args.length)));
        } else {
            System.out.println("Error");
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
            Class<?>[] argClasses = getClassArray(constructorArgs);
            c.getConstructor(argClasses);
        } catch (Exception e) {
            return false;
        }
        return true;
    }

    
    public static boolean queryMethod(String classFullName, String methodName, String... methodArgs) {
        try {
            Class<?> c = Class.forName(classFullName);
            Class<?>[] argClasses = getClassArray(methodArgs);
            c.getMethod(methodName, argClasses);
        } catch (Exception e) {
            return false;
        }
        return true;
    }
    

    private static Class<?>[] getClassArray(String[] names) throws ClassNotFoundException {
        Class<?>[] classes = new Class<?>[names.length];
        for (int i = 0; i < names.length; i++) {
            classes[i] = Class.forName(names[i]);
        }
        return classes;
    }
}
