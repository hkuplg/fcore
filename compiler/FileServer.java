import java.util.*;
import java.lang.*;
import java.io.*;
import javax.tools.*;
import java.net.*;
import java.lang.reflect.*;

public class FileServer {

    public final static int FILE_SIZE = 1022386;

    public static void compileLoad (String fileName)
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

        String [] compileOptions = new String[] {"-classpath", "runtime.jar"};
        Iterable<String> compilationOptions = Arrays.asList(compileOptions);

        JavaCompiler.CompilationTask task =
            compiler.getTask(null, fileManager, diagnostics, compilationOptions, 
                    null, compilationUnits);
        task.call();
        //compiler.run(null, null, null, sourceFile.getPath());

        String className = "";
        int i = 0;
        while(fileName.charAt(i) != '.') {
            className += fileName.charAt(i);
            i++;
        }

        // Dynamically load class and invoke its main method.
        try {
            Class<?> cls = Class.forName(className);
            Method meth = cls.getMethod("main", String[].class);
            String[] params = null;
            meth.invoke(null, (Object) params);
        } catch (Exception e) {
            e.printStackTrace();     
        }
    }

    public static void main (String [] args)
    {
        Scanner scanner = new Scanner(System.in);

	while(true){
	  if(!scanner.hasNextLine()) break;

          String fileName = scanner.nextLine();
          System.out.println(fileName);

        // Receive .java file from client
          try {
            File myFile = new File(fileName);
            BufferedWriter output = new BufferedWriter(new FileWriter(myFile));
            
	    while (scanner.hasNextLine()) {
	        String line = scanner.nextLine();
		if(line.equals("//end of file")) break;
                output.write(line);
		//System.out.println(line);
            }
            output.close();
          } catch (Exception e) {
            e.printStackTrace();
          }

          compileLoad(fileName);
          System.out.println("exit");
	}

    }

}
