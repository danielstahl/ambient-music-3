package net.soundmining

import de.sciss.osc.UDP.Receiver
import de.sciss.osc.{Message, Packet, PacketCodec, UDP}
import net.soundmining.Generative.{MarkovChain, Picker, WeightedRandom}
import net.soundmining.modular.ModularSynth.{lineControl, percControl, relativePercControl, staticControl}
import net.soundmining.modular.SynthPlayer
import net.soundmining.synth.{Instrument, SuperColliderClient}
import net.soundmining.synth.SuperColliderClient.loadDir

import java.net.SocketAddress
import scala.util.Random

/**
 * Mix noise and pitched tones. Maybe with a MarkovChain between noise and pitch
 * and a second layer of MarkovChain to choose which noise and pitch.
 *
 */
object AmbientMusic3 {

  implicit val client: SuperColliderClient = SuperColliderClient()
  val SYNTH_DIR = "/Users/danielstahl/Documents/Projects/soundmining-modular/src/main/sc/synths"
  val synthPlayer = SynthPlayer(soundPlays = Map.empty, numberOfOutputBuses = 64)
  var midiServer: Receiver.Undirected = _
  implicit val random: Random = new Random()

  def init(): Unit = {
    println("Starting up SuperCollider client")
    client.start
    Instrument.setupNodes(client)
    client.send(loadDir(SYNTH_DIR))
    synthPlayer.init()

    val cfg = UDP.Config()
    cfg.codec = PacketCodec().doublesAsFloats().booleansAsInts()
    cfg.localPort = 57111
    this.midiServer = UDP.Receiver(cfg)
    this.midiServer.connect()
    this.midiServer.action = midiReply
  }

  val NOTE_NAMES = Seq("c", "ciss", "d", "diss", "e", "f", "fiss", "g", "giss", "a", "aiss", "h")

  def midiReply(packet: Packet, socketAddress: SocketAddress): Unit = {
    packet match {
      case Message("/noteOn", key: Int, velocity: Int) =>
        noteHandle(key, velocity)
      case _ =>
    }
  }

  def stop(): Unit = {
    println("Stopping SuperCollider client")
    client.stop
    this.midiServer.close()
  }

  var lowNoiseFundamental: Double = Note.noteToHertz("c1")
  var lowNoiseSecond: Double = Note.noteToHertz("fiss1")

  def lowNoiseNote(start: Double = 0, noteSuggestion: Int, spread: Int, length: Double, pan: Double, velocity: Double, attack: Seq[Double], spect: (Double, Double) = (lowNoiseFundamental, lowNoiseSecond)): Unit = {
    val (fund, sec) = spect
    val fact = Spectrum.makeFact(fund, sec)

    val note = math.max(noteSuggestion, spread)
    val spectrum = Spectrum.makeSpectrum2(fund, fact, 50)

    println(s"low noise start $start note $note spread $spread pitch ${spectrum(note - spread)} ${spectrum(note)} ${spectrum(note + spread)} length $length pan $pan velocity $velocity attack $attack spect $spect")

    synthPlayer()
      .whiteNoise(relativePercControl(0.001, velocity * 2, attack.head, Right(Instrument.SINE)))
      .lowPass(lineControl(spectrum(note), spectrum(note + spread)))
      .highPass(lineControl(spectrum(note), spectrum(note - spread)))
      .pan(lineControl(pan + 0.3, pan - 0.3))
      .playWithDuration(start, length, outputBus = 0)

    synthPlayer()
      .pinkNoise(relativePercControl(0.001, velocity * 2, attack(1), Right(Instrument.SINE)))
      .lowPass(lineControl(spectrum(note + spread), spectrum(note)))
      .highPass(lineControl(spectrum(note - spread), spectrum(note)))
      .pan(lineControl(pan - 0.3, pan + 0.3))
      .playWithDuration(start, length, outputBus = 2)
  }

  var lowPitchFundamental: Double = Note.noteToHertz("d1")
  var lowPitchSecond: Double = Note.noteToHertz("giss2")

  def lowPitchNote(start: Double = 0, note: Int, length: Double, pan: Double, velocity: Double, attack: Seq[Double], spect: (Double, Double) = (lowPitchFundamental, lowPitchSecond)): Unit = {
    val (fund, sec) = spect
    val fact = Spectrum.makeFact(fund, sec)

    val spectrum = Spectrum.makeSpectrum2(fund, fact, 50)
    val spectrumNote = spectrum(note)

    println(s"low pitch start $start note $note note pitch ${spectrum(note)} ${spectrum(note + 2)} ${spectrum(note + 3)} ${spectrum(note + 4)} length $length pan $pan velocity $velocity attack $attack spect $spect")

    synthPlayer()
      .saw(staticControl(spectrumNote), relativePercControl(0.001, velocity, attack.head, Right(Instrument.WELCH)))
      .ring(staticControl(spectrum(note)))
      .lowPass(staticControl(spectrum(note)))
      .pan(staticControl(pan - 0.1))
      .playWithDuration(start, length, outputBus = 4)

    synthPlayer()
      .triangle(staticControl(spectrumNote), relativePercControl(0.001, velocity, attack(1), Right(Instrument.WELCH)))
      .ring(staticControl(spectrum(note + 2)))
      .lowPass(staticControl(spectrum(note)))
      .pan(staticControl(pan + 0.1))
      .playWithDuration(start, length, outputBus = 6)

    synthPlayer()
      .pinkNoise(relativePercControl(0.001, velocity * 10, attack(2), Right(Instrument.WELCH)))
      .bandPass(staticControl(spectrumNote), staticControl(spectrumNote / 5000.0))
      .ring(staticControl(spectrum(note + 1)))
      .lowPass(staticControl(spectrum(note + 1)))
      .pan(lineControl(pan - 0.2, pan + 0.2))
      .playWithDuration(start, length, outputBus = 8)
  }

  var middleNoiseFundamental: Double = Note.noteToHertz("c4")
  var middleNoiseSecond: Double = Note.noteToHertz("fiss4")

  def middleNoiseNote(start: Double = 0, note: Int, spread: Int, length: Double, pan: Double, velocity: Double, attack: Seq[Double], spect: (Double, Double) = (middleNoiseFundamental, middleNoiseSecond)): Unit = {

    val (fund, sec) = spect
    val fact = Spectrum.makeFact(fund, sec)

    val spectrum = Spectrum.makeSpectrum2(fund, fact, 50)

    println(s"middle noise start $start note $note spread $spread pitch ${spectrum(note + spread)} ${spectrum(note)} length $length pan $pan velocity $velocity attack $attack spect $spect")

    synthPlayer()
      .pinkNoise(relativePercControl(0.001, velocity, attack.head, Right(Instrument.WELCH)))
      .bandPass(
        lineControl(spectrum(note), spectrum(note + spread)),
        lineControl(400.0 / 1000.0, 200.0 / 1000.0))
      .pan(lineControl(pan - 0.5, pan + 0.5))
      .playWithDuration(start, length, outputBus = 10)

    synthPlayer()
      .whiteNoise(relativePercControl(0.001, velocity, attack(1), Right(Instrument.WELCH)))
      .bandPass(
        lineControl(spectrum(note + spread), spectrum(note)),
        lineControl(200.0 / 1000.0, 400.0 / 1000.0))
      .pan(lineControl(pan + 0.5, pan - 0.5))
      .playWithDuration(start, length, outputBus = 12)
  }

  var middlePitchFundamental: Double = Note.noteToHertz("aiss1")
  var middlePitchSecond: Double = Note.noteToHertz("e3")

  def middlePitchNote(start: Double = 0, note: Int, length: Double, pan: Double, velocity: Double, attack: Seq[Double], spect: (Double, Double) = (middlePitchFundamental, middlePitchSecond)): Unit = {
    val (fund, sec) = spect
    val fact = Spectrum.makeFact(fund, sec)

    val spectrum = Spectrum.makeSpectrum2(fund, fact, 50)
    val spectrumNote = spectrum(note)

    println(s"middle pitch start $start note $note note pitch ${spectrum(note)} ${spectrum(note + 2)} ${spectrum(note + 3)} ${spectrum(note + 4)} length $length pan $pan velocity $velocity attack $attack spect $spect")

    synthPlayer()
      .pulse(staticControl(spectrumNote), relativePercControl(0.001, velocity, attack.head, Right(Instrument.WELCH)))
      .ring(staticControl(spectrum(note)))
      .highPass(staticControl(spectrum(note)))
      .lowPass(staticControl(spectrum(note + 1)))
      .pan(staticControl(pan - 0.4))
      .playWithDuration(start, length, outputBus = 14)

    synthPlayer()
      .sine(staticControl(spectrumNote), relativePercControl(0.001, velocity, attack(1), Right(Instrument.WELCH)))
      .ring(staticControl(spectrum(note + 1)))
      .highPass(staticControl(spectrum(note + 3)))
      .lowPass(staticControl(spectrum(note)))
      .pan(staticControl(pan + 0.4))
      .playWithDuration(start, length, outputBus = 16)

    synthPlayer()
      .pinkNoise(relativePercControl(0.001, velocity * 10, attack(2), Right(Instrument.WELCH)))
      .bandPass(staticControl(spectrumNote), staticControl(spectrumNote / 5000.0))
      .ring(staticControl(spectrum(note + 2)))
      .lowPass(staticControl(spectrum(note + 1)))
      .highPass(staticControl(spectrum(note)))
      .pan(lineControl(pan - 0.4, pan + 0.4))
      .playWithDuration(start, length, outputBus = 18)
  }

  def noteHandle(key: Int, velocity: Int): Unit = {
    val octave = (key / 12) - 1
    val note = key % 24

    octave match {
      case 2 => lowNoteHandle(key - 12, velocity)
      case 3 => middleNoteHandle(key - 12, velocity)
      case _ =>

    }
  }

  def lowNoteHandle(key: Int, velocity: Int): Unit = {
    if (client.clockTime <= 0) client.resetClock
    val start = (System.currentTimeMillis() - (client.clockTime + 1900)) / 1000.0
    val baseLen = shortLengths.choose()
    val attack = attacks.pick(3)
    val pan = (random.nextDouble() * 2) - 1

    noisePitch.choose() match {
      case 0 =>
        lowPitchNote(start, key % 12, math.min((127.0 / velocity) * baseLen, 35), pan, velocity / 127.0, attack)
      case 1 =>
        lowNoiseNote(start, key % 12, lowSpreads.choose(), math.min((127.0 / velocity) * baseLen, 35), pan, velocity / 127.0, attack)
    }
  }

  def middleNoteHandle(key: Int, velocity: Int): Unit = {
    if (client.clockTime <= 0) client.resetClock
    val start = (System.currentTimeMillis() - (client.clockTime + 1900)) / 1000.0
    val baseLen = shortLengths.choose()
    val attack = attacks.pick(4)
    val pan = (random.nextDouble() * 2) - 1

    noisePitch.choose() match {
    case 0 =>
      middlePitchNote(start, key % 12, math.min((127.0 / velocity) * baseLen, 35), pan, velocity / 127.0, attack)
    case 1 =>
      middleNoiseNote(start, key % 12, middleSpreads.choose(), math.min((127.0 / velocity) * baseLen, 35), pan, velocity / 127.0, attack)
    }
  }

  def limitLength(len: Double): Double =
    len match {
      case normal if normal < lengthLimit => normal
      case long if long > 60 =>
        println(s"$long is to long")
        limitLength(long * (0.5 + random.nextDouble() * 0.5) )
    }

  def evenMarkovChain[T](values: Seq[T], startValue: T): MarkovChain[T] = {
    val rate = 1.0 / (values.size - 1)
    val nodes = values.map(value => (value, values.filter(_ != value).map((_, rate)))).toMap
    MarkovChain(nodes, startValue)
  }

  val totalDuration = 13 * 60

  val velocities = WeightedRandom(Seq(("low", 0.1), ("middle", 0.8), ("high", 0.1)))
  val lengthLimit = 60
  val timeConstant = 1.0
  val ampConstant = 2.0

  val noisePitch = WeightedRandom(Seq((0, 0.2), (1, 0.8)))

  val shortLengths = WeightedRandom(Seq((13, 0.2), (8, 0.3), (5, 0.5)))
  val middleLengths = WeightedRandom(Seq((21, 0.1), (13, 0.7), (8, 0.2)))

  val attacks = Picker(Seq(0.3, 0.4, 0.5, 0.6, 0.7))
  val lowSpreads = WeightedRandom(Seq((1, 0.5), (2, 0.3), (3, 0.2)))

  val middleSpreads = WeightedRandom(Seq((2, 0.5), (3, 0.3), (4, 0.2)))

  val lowNoteChain = evenMarkovChain(Seq(0, 1, 2, 3, 4, 5, 6, 7, 8 ,9, 10, 11, 12), 0)
  val middleNoteChain = evenMarkovChain(Seq(0, 1, 2, 3, 4, 5, 6, 7, 8 ,9, 10, 11, 12), 0)


  def chooseVelocity: Double =
    velocities.choose() match {
      case "low" => (random.nextDouble() * 0.2) + 0.1
      case "middle" => (random.nextDouble() * 0.6) + 0.2
      case "high" => (random.nextDouble() * 0.2) + 0.8 - 0.1
    }

  def playOne(start: Double = 0, reset: Boolean = true): Unit = {
    if (reset) client.resetClock

    var lowTime = start
    println(s"low start $lowTime")
    while (lowTime < totalDuration) {
      val velocity = chooseVelocity

      val baseTime = shortLengths.choose()
      val time = limitLength((timeConstant / velocity) * baseTime)
      val baseLen = middleLengths.choose()
      val length = limitLength((timeConstant / velocity) * baseLen)
      val attack = attacks.pick(4)
      val note = lowNoteChain.next
      val pan = (random.nextDouble() * 2) - 1

      noisePitch.choose() match {
        case 0 =>
          lowPitchNote(lowTime, note, length, pan, velocity * ampConstant, attack)
        case 1 =>
          lowNoiseNote(lowTime, note, lowSpreads.choose(), length, pan, velocity * ampConstant, attack)
      }
      lowTime += time
    }

    var middleTime = start + (13 * (0.5 + random.nextDouble()))
    println(s"middle start $middleTime")
    while(middleTime < totalDuration) {
      val velocity = chooseVelocity

      val baseTime = shortLengths.choose()
      val time = limitLength((timeConstant / velocity) * baseTime)
      val baseLen = middleLengths.choose()
      val length = limitLength((timeConstant / velocity) * baseLen)
      val attack = attacks.pick(4)
      val note = middleNoteChain.next
      val pan = (random.nextDouble() * 2) - 1

      noisePitch.choose() match {
        case 0 =>
          middlePitchNote(middleTime, note, length, pan, velocity * ampConstant, attack)
        case 1 =>
          middleNoiseNote(middleTime, note, middleSpreads.choose(), length, pan, velocity * ampConstant, attack)
      }
      middleTime += time
    }
  }
}
