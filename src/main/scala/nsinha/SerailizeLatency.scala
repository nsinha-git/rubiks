package nsinha

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}

import com.esotericsoftware.kryo.Kryo
import com.esotericsoftware.kryo.io.{Input, Output}


object SerailizeLatency {


  def main(args: Array[String]): Unit = {
    //serializeKryo(10)
    serializeJava(10)
    serializeJava(10)
    serializeJava(10)
  }


  def serializeJava(n :Int): Unit = {
    val t = TestJavaClass.getInstance(n)
    val baos = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(baos)


    val t0 = System.nanoTime()

    for (i <- Range(0, 1)) {
      oos.writeObject(t)
    }

    val t1 = System.nanoTime()

    val diff = t1 - t0

    println(diff)


    val baios = new ByteArrayInputStream(baos.toByteArray)
    val ios = new ObjectInputStream(baios)

    val t2 = System.nanoTime()
    for (i <- Range(0, 1)) {
      val o = ios.readObject()
      o.hashCode()
    }
    val t3 = System.nanoTime()
    val diff1 = t3 - t2
    println(diff1)


  }

  def serializeKryo(n :Int): Unit = {

    val t = TestJavaClass.getInstance(n)

    val baos = new ByteArrayOutputStream()





    val kryo = new Kryo()

    val output = new Output(baos)


    val t0 = System.nanoTime()

    for (i <- Range(0, 1)) {
      kryo.writeObject(output, t)
    }

    val t1 = System.nanoTime()

    val diff = t1 - t0

    println(diff)


    val baios = new ByteArrayInputStream(baos.toByteArray)
    val input = new Input(baios)

    val t2 = System.nanoTime()
    for (i <- Range(0, 1)) {
      val o = kryo.readClass(input)
      o.hashCode()
    }
    val t3 = System.nanoTime()
    val diff1 = t3 - t2
    println(diff1)


  }




}
