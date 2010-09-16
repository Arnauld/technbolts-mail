import sbt._

class MailProject(info: ProjectInfo) extends DefaultProject(info) {

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   * dependencies
   * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  object Version {
    val log4j = "1.2.15"
    val slf4j = "1.6.1"
    val scala = "2.8.0"
    val commonsIO = "1.4"
    val mail = "1.4.1"
    val spring = "3.0.4.RELEASE"
    //
    val junit = "4.8.1"
    val scala_specs = "1.6.5"
    val mockito = "1.8.5"
  }

  // Use ivy directly to use exclude behavior
  override def ivyXML =
    <dependencies>
      <!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->
      <!-- Commons: Http, IO, ...       -->
      <!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->
        <dependency org="commons-io" name="commons-io" rev={Version.commonsIO}/>

      <!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->
      <!--     mail       -->
      <!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->
        <dependency org="javax.mail" name="mail" rev={Version.mail}/>

      <!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->
      <!-- Test -->
      <!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->
        <dependency org="junit" name="junit" rev={Version.junit} conf="test->default"/>
        <dependency org="org.scala-tools.testing" name={"specs_" + Version.scala} rev={Version.scala_specs} conf="test->default"/>
        <dependency org="org.mockito" name="mockito-all" rev={Version.mockito} />

      <!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->
      <!-- Spring -->
      <!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->
        <dependency org="org.springframework" name="spring-core" rev={Version.spring} conf="test->default"/>
        <dependency org="org.springframework" name="spring-context-support" rev={Version.spring} conf="test->default"/>


      <!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->
      <!-- logs -->
      <!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->
        <dependency org="org.slf4j" name="slf4j-api" rev={Version.slf4j}/>
        <dependency org="org.slf4j" name="slf4j-log4j12" rev={Version.slf4j}/>
      <dependency org="log4j" name="log4j" rev={Version.log4j}>
          <exclude name="mail"/>
          <exclude name="jms"/>
          <exclude name="jmxtools"/>
          <exclude name="jmxri"/>
      </dependency>
    </dependencies>
}
