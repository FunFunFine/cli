val zioVersion = "2.0.0-M6-2"

scalaVersion := "3.1.1-RC1"

libraryDependencies ++= Seq(
  "dev.zio" %% "zio"          % zioVersion,
  "dev.zio" %% "zio-streams"          % zioVersion,
  "dev.zio" %% "zio-test"     % zioVersion % Test,
  "dev.zio" %% "zio-test-sbt" % zioVersion % Test,
  "eu.timepit" %% "refined" % "0.9.28",
  "org.typelevel" %% "cats-core" % "2.7.0"

)
resolvers += Resolver.sonatypeRepo("snapshots")
libraryDependencies += "dev.zio" %% "zio-prelude" % "1.0.0-RC8"

testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
