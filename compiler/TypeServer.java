import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.ServerSocket;
import java.net.Socket;
import java.nio.charset.Charset;
import java.util.Arrays;



public class TypeServer {

    public static final int PORT_NUM = 12345;

    public static void main(String[] args) {
        try {
            ServerSocket ss = new ServerSocket(PORT_NUM);
            println("Ready!");
            while (true) {
                println("Waiting for client...");
                Socket s = ss.accept();
                println("New client from: " + s.toString());
                new ServerThread(s).start();
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }


    private static void print(String str) {
        System.out.print("Thread Main> " + str);
    }


    private static void println(String str) {
        print(str + "\n");
    }
}



class ServerThread extends Thread {

    public static final char END_CHAR = '$';

    private Socket s;
    private InputStream is;
    private OutputStream os;
    private byte[] buf = new byte[1024];
    private int len;
    

    public ServerThread(Socket s) {
        this.s = s;
        try {
            is = s.getInputStream();
            os = s.getOutputStream();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
    

    public void run() {
        len = 0;
        try {
            while (true) {
                int b = is.read();
                if (b == -1) continue;
                //System.out.println(b);
                if ((char)b == END_CHAR) {
                    os.write(gao());
                    os.flush();
                    len = 0;
                }
                else {
                    buf[len++] = (byte)b;
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }


    private byte[] gao() {
        String data = new String(buf, 0, len, Charset.forName("UTF-8"));
        
        println("Recv: " + data);
        
        String[] args = data.split(" ");

        //for(int i = 0; i < words.length; i++) System.out.println(words[i]);

        Object result = false;

        if (args[0].equals("qType")) {
            result = Worker.queryType(args[1]);

        } else if (args[0].equals("qConstructor")) {
            result = Worker.queryNew(args[1], Arrays.copyOfRange(args, 2, args.length));

        } else if (args[0].equals("qMethod")) {
            result = Worker.queryMethod(args[1], args[2], Arrays.copyOfRange(args, 3, args.length));

        } else {
            println("######## BAD QUERY ########");

        }

        byte[] r = result.toString().getBytes();
        //byte[] r = result ? "true".getBytes() : "false".getBytes();
        println("Send: " + new String(r));

        return r;
    }


    private void print(String str) {
        System.out.print("Thread #" + s.getPort() + "> " + str);
    }


    private void println(String str) {
        print(str + "\n");
    }
}



class Worker {

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