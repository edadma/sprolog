name := "sprolog"

version := "0.2"

scalaVersion := "2.12.1"

crossScalaVersions := Seq( "2.11.8" )

isSnapshot := true

scalacOptions ++= Seq( "-deprecation", "-feature", "-language:postfixOps", "-language:implicitConversions", "-language:existentials" )

incOptions := incOptions.value.withNameHashing( true )

organization := "xyz.hyperreal"

//resolvers += Resolver.sonatypeRepo( "snapshots" )

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

resolvers += "Hyperreal Repository" at "https://dl.bintray.com/edadma/maven"

libraryDependencies ++= Seq(
	"org.scalatest" %% "scalatest" % "3.0.0" % "test",
	"org.scalacheck" %% "scalacheck" % "1.13.4" % "test"
)

libraryDependencies ++= Seq(
	"xyz.hyperreal" %% "lia" % "0.20",
	"xyz.hyperreal" %% "rtcep" % "0.3"
	)
	
//mainClass in (Compile, packageBin) := Some( "xyz.hyperreal.myproject.Main" )

mainClass in (Compile, run) := Some( "xyz.hyperreal.sprolog.TestMain" )

//offline := true

mainClass in assembly := Some( "xyz.hyperreal.myproject.Main" )

assemblyJarName in assembly := name.value + "-" + version.value + ".jar"


publishMavenStyle := true

//publishTo := Some( Resolver.sftp( "private", "hyperreal.ca", "/var/www/hyperreal.ca/maven2" ) )

//{
//  val nexus = "https://oss.sonatype.org/"
//  if (isSnapshot.value)
//    Some("snapshots" at nexus + "content/repositories/snapshots")
//  else
//    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
//}

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

licenses := Seq("MIT" -> url("http://opensource.org/licenses/MIT"))

homepage := Some(url("https://github.com/edadma/sprolog"))

pomExtra := (
  <scm>
    <url>git@github.com:edadma/sprolog.git</url>
    <connection>scm:git:git@github.com:edadma/sprolog.git</connection>
  </scm>
  <developers>
    <developer>
      <id>edadma</id>
      <name>Edward A. Maxedon, Sr.</name>
      <url>http://hyperreal.ca</url>
    </developer>
  </developers>)
