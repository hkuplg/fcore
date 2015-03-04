package f2j; 

import java.util.*;
import java.lang.*;
import java.io.*;
import javax.tools.*;
import java.net.*;
import java.lang.reflect.*;

public class FileServer {

    public final static int FILE_SIZE = 1022386;

    public static void compile(String fileName, String cp)
    {
	      // Save source in .java file.
        File sourceFile = new File(fileName);

        // Compile source file.
        JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();

        DiagnosticCollector <JavaFileObject> diagnostics =
            new DiagnosticCollector<JavaFileObject>();
        StandardJavaFileManager fileManager = 
            compiler.getStandardFileManager(diagnostics, null, null);  
        File [] files = new File [] {sourceFile};
        Iterable<? extends JavaFileObject> compilationUnits =
            fileManager.getJavaFileObjectsFromFiles(Arrays.asList(files));

        String [] compileOptions = new String[] {"-classpath", cp};
        Iterable<String> compilationOptions = Arrays.asList(compileOptions);

        JavaCompiler.CompilationTask task =
        compiler.getTask(null, fileManager, diagnostics, compilationOptions, 
                         null, compilationUnits);
        task.call();
    }

    public static String compileLoad (String fileName, String cp) 
      throws InvocationTargetException, 
             ClassNotFoundException, 
             NoSuchMethodException, 
             IllegalAccessException 
    {
        compile(fileName,cp);

        String className = "";
        int i = 0;
        while(fileName.charAt(i) != '.') {
            className += fileName.charAt(i);
            i++;
        }

      	ClassLoader classLoader = FileServer.class.getClassLoader();
        // Dynamically load class and invoke its main method.
        try {
            //Class<?> cls = Class.forName(className);
	          Class<?> cls = classLoader.loadClass(className);
            Method meth = cls.getMethod("main", String[].class);
            String[] params = null;

	        try {
	          meth.setAccessible(true);
            meth.invoke(null, (Object) params);
	        } catch (InvocationTargetException e) {
            System.out.println(e + ": " + e.getCause());
	        }
        } catch (ClassNotFoundException e1) {
          System.out.println(e1 + ": " + e1.getCause());
        } catch (NoSuchMethodException e2) {
          System.out.println(e2 + ": " + e2.getCause());
        } catch (IllegalAccessException e3) {
          System.out.println(e3 + ": " + e3.getCause());
        }
        System.out.println("exit");

  	    return className;
    }

    public static void DeleteDummy()
    {
      File dir = new File(".");
      File fList[] = dir.listFiles();

      for(File f : fList) {
      	if(f.getName().endsWith(".class") 
           && !f.getName().startsWith("FileServer")
           && !f.getName().startsWith("TypeServer"))
	        f.delete();
      }      
    }

    public static void main (String [] args)
    {
        Scanner scanner = new Scanner(System.in);
        String cp = args[0];

	      while(true){
	        if(!scanner.hasNextLine()) break;

          String fileName = scanner.nextLine();

          //Receive .java file from client
          try {
            File myFile = new File(fileName);
            BufferedWriter output = new BufferedWriter(new FileWriter(myFile));
            
	          while (scanner.hasNextLine()) {
	            String line = scanner.nextLine();
		          if(line.equals("//end of file")) break;
              output.write(line);
              }
              output.close();
	            String className = compileLoad(fileName,cp);
	            myFile.delete();
          } catch (Exception e) {
            e.printStackTrace();
          }

	        // Delete dummy .class files in current directory
	        DeleteDummy();
	      } 
    }

}
