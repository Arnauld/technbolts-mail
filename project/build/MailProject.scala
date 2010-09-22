import sbt._

class MailProject(info: ProjectInfo) extends DefaultProject(info) {

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   * dependencies
   * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  object Version {
    val log4j = "1.2.15"
    val slf4j = "1.6.1"
    val scala = "2.8.0"
    //
    val commonsIO = "1.4"
    val mail = "1.4.1"
    val spring = "3.0.4.RELEASE"
    val netty = "3.2.2.Final"
    //
    val junit = "4.8.1"
    val scala_specs = "1.6.5"
    val mockito = "1.8.5"
  }

  val jbossRepository = "repository.jboss.org" at "http://repository.jboss.org/nexus/content/groups/public/"

  // Use ivy directly to use exclude behavior
  override def ivyXML =
    <dependencies>
      <!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->
      <!-- Commons: Http, IO, ...       -->
      <!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->
        <dependency org="commons-io" name="commons-io" rev={Version.commonsIO}/>
        <dependency org="javax.mail" name="mail" rev={Version.mail}/>
        <dependency org="org.jboss.netty" name="netty" rev={Version.netty} />

      <!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->
      <!--     Spring       -->
      <!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->
        <dependency org="org.springframework" name="spring-core" rev={Version.spring}/>
        <dependency org="org.springframework" name="spring-context-support" rev={Version.spring} conf="test->default"/>

      <!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->
      <!-- Test -->
      <!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->
        <dependency org="junit" name="junit" rev={Version.junit} conf="test->default"/>
        <dependency org="org.scala-tools.testing" name={"specs_" + Version.scala} rev={Version.scala_specs} conf="test->default"/>
        <dependency org="org.mockito" name="mockito-all" rev={Version.mockito}/>

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
