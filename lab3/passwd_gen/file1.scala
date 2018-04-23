
package utils {
    object PasswdGen {
        val allowedChars = 
            (('0' to '9') ++ ('A' to 'Z') ++ ('a' to 'z')).toArray ++ Array('$', '-', '.', '_')
        val allowedCharsLength = allowedChars.length
        def nextPasswd(passwdLen: Int):String = {
            val passwd = new StringBuilder(passwdLen)
            for (i <- 0 to passwdLen)
                passwd += nextChar()
            passwd.toString
        }
        def nextChar():Char = {
            allowedChars(util.Random.nextInt(allowedCharsLength))
        }
    }
}