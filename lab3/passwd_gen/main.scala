object Appl {
  def generatePasswd(inFileName: String, outFileName: String) = try {
    import utils._
    val outFile = new java.io.PrintWriter(outFileName)
    val inFile = scala.io.Source.fromFile(inFileName)
    try {
      for (login <- inFile.getLines)
          outFile.println(login + ":" + PasswdGen.nextPasswd(5))
    } finally {
      outFile.close
      inFile.close
    }
  } catch {
    case ex: java.io.FileNotFoundException => println(ex.getMessage)
    case ex: Throwable => println("Default exception handler: "+ ex.getMessage)
  }
 
  def main(args: Array[String]) {
    generatePasswd("logins.txt", "logins_passwd.txt")
  }   
}