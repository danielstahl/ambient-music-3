package net.soundmining

import net.soundmining.AmbientMusic3.{client, synthPlayer}
import net.soundmining.modular.ModularSynth.{lineControl, percControl, relativePercControl, staticControl}

object Test {

  def testPluck(start: Double = 0, reset: Boolean = true): Unit = {
    if(reset) client.resetClock

    val freq = Note.noteToHertz("c4")

    synthPlayer()
      //.whiteNoise(percControl(0, 1, 0.05, Right(Instrument.SINE)), Some(0.01))
      .whiteNoise(percControl(0, 1, 0, Left(-4)), Some(0.001))
      .delay(1.0 / freq, 5.0, staticControl(1))
      .pan(staticControl(0))
      .playWithDuration(0, 5.0)
  }


  def testNoise(start: Double = 0, reset: Boolean = true): Unit = {
    if(reset) client.resetClock

    synthPlayer()
      .pinkNoise(relativePercControl(0.001, 1, 0.5, Left(0)))
      .lowPass(relativePercControl(100, 500, 0.5, Left(0)))
      .pan(staticControl(0))
      .playWithDuration(start, 5)


    synthPlayer()
      .pinkNoise(relativePercControl(0.001, 1, 0.5, Left(0)))
      .highPass(relativePercControl(9000, 5000, 0.5, Left(0)))
      .pan(staticControl(0))
      .playWithDuration(start + 3, 5)


    synthPlayer()
      .pinkNoise(relativePercControl(0.001, 1, 0.5, Left(0)))
      .bandPass(
        relativePercControl(1000, 3000, 0.5, Left(0)),
        relativePercControl(400.0 / 1000.0, 200.0 / 1000.0, 0.5, Left(0)))
      .pan(staticControl(0))
      .playWithDuration(start + 8, 5)
  }

  def testNoiseVariants(start: Double = 0, reset: Boolean = true): Unit = {
    if(reset) client.resetClock

    synthPlayer()
      .whiteNoise(relativePercControl(0.001, 1, 0.5, Left(0)))
      .lowPass(relativePercControl(50, 100, 0.5, Left(0)))
      .highPass(relativePercControl(50, 40, 0.5, Left(0)))
      .pan(lineControl(0.9, -0.9))
      .playWithDuration(start, 13)

    synthPlayer()
      .pinkNoise(relativePercControl(0.001, 1, 0.5, Left(0)))
      .lowPass(relativePercControl(100, 50, 0.5, Left(0)))
      .highPass(relativePercControl(40, 50, 0.5, Left(0)))
      .pan(lineControl(-0.9, 0.9))
      .playWithDuration(start, 13)

    synthPlayer()
      .whiteNoise(relativePercControl(0.001, 1, 0.5, Left(0)))
      .lowPass(relativePercControl(400, 500, 0.5, Left(0)))
      .highPass(relativePercControl(400, 300, 0.5, Left(0)))
      .pan(lineControl(0.5, -0.5))
      .playWithDuration(start + 8, 13)

    synthPlayer()
      .pinkNoise(relativePercControl(0.001, 1, 0.5, Left(0)))
      .lowPass(relativePercControl(500, 400, 0.5, Left(0)))
      .highPass(relativePercControl(300, 400, 0.5, Left(0)))
      .pan(lineControl(-0.5, 0.5))
      .playWithDuration(start + 8, 13)
  }
}
