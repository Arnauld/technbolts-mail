package runner

import org.technbolts.mail.smtp.SmtpServer
import org.technbolts.util.LangUtils
import io.Source

object SmtpServerRunner {
  def main(args: Array[String]):Unit = {
    System.out.print("Smtp port [26]: ")
    val line = Source.fromInputStream(System.in).getLines.next
    val port = if(LangUtils.isEmpty(line)) 26 else Integer.parseInt(line)
    SmtpServer(port).start
  }
}