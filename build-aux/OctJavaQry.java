// Code used by configure script to locate Java installation variables.
// Only compiled code, OctJavaQry.class, is distributed.
public class OctJavaQry
{
  public static void main (String[] args)
  {
    if (args.length > 0)
    {
      if (args[0].equals ("JAVA_HOME"))
      {
        System.out.println (System.getProperty ("java.home"));
      }
      else if (args[0].equals ("JAVA_LDPATH"))
      {
        System.out.println (System.getProperty ("java.library.path"));
      }
      else if (args[0].equals ("JAVA_BOOTPATH"))
      {
        System.out.println (System.getProperty ("sun.boot.library.path"));
      }
    }
  }
}
