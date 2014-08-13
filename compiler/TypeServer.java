import java.util.Scanner;
import java.util.Arrays;


public class TypeServer {

    public static void main(String[] args) {
        Scanner inp = new Scanner(System.in);
        while (true) {
            if (!inp.hasNextLine()) continue;
            String line = inp.nextLine();
            String[] words = line.split(" ");
            Object result = false;
            if (words[0].equals("qType")) {
                result = queryType(words[1]);
            } else if (words[0].equals("qConstructor")) {
                result = queryNew(words[1], Arrays.copyOfRange(words, 2, words.length));
            } else if (words[0].equals("qMethod")) {
                result = queryMethod(words[1], words[2], Arrays.copyOfRange(words, 3, words.length));
            } else {
                System.err.println("######## BAD QUERY ########");
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
            Class<?>[] argClasses = getClassArray(constructorArgs);
            c.getConstructor(argClasses);
        } catch (Exception e) {
            return false;
        }
        return true;
    }


    public static String queryMethod(String classFullName, String methodName, String... methodArgs) {
        String ret;
        try {
            Class<?> c = Class.forName(classFullName);
            Class<?>[] argClasses = getClassArray(methodArgs);
            ret = c.getMethod(methodName, argClasses).getReturnType().getName();
        } catch (Exception e) {
            return "$";
        }
        return ret;
    }


    private static Class<?>[] getClassArray(String[] names) throws ClassNotFoundException {
        Class<?>[] classes = new Class<?>[names.length];
        for (int i = 0; i < names.length; i++) {
            classes[i] = Class.forName(names[i]);
        }
        return classes;
    }
}

