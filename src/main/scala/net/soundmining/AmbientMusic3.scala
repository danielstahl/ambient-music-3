package net.soundmining

import de.sciss.osc.UDP.Receiver
import de.sciss.osc.{Message, Packet, PacketCodec, UDP}
import net.soundmining.Generative.{Picker, WeightedRandom}
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
  val synthPlayer = SynthPlayer(soundPlays = Map.empty, numberOfOutputBuses = 2)
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

  var pitchFundamental: Double = Note.noteToHertz("d1")
  var pitchSecond: Double = Note.noteToHertz("giss2")

  def pitchNote(start: Double = 0, note: Int, length: Double, pan: Double, velocity: Double, attack: Seq[Double], spect: (Double, Double) = (pitchFundamental, pitchSecond)): Unit = {
    val (fund, sec) = spect
    val fact = Spectrum.makeFact(fund, sec)

    val spectrum = Spectrum.makeSpectrum2(fund, fact, 50)
    val spectrumNote = spectrum(note)

    println(s"pitch note $note note pitch ${spectrum(note)} ${spectrum(note + 2)} ${spectrum(note + 3)} ${spectrum(note + 4)} length $length pan $pan velocity $velocity attack $attack spect $spect")

    synthPlayer()
      .saw(staticControl(spectrumNote), relativePercControl(0.001, velocity / 50, attack.head, Right(Instrument.WELCH)))
      .ring(staticControl(spectrum(note)))
      .lowPass(staticControl(spectrum(note)))
      .pan(staticControl(pan - 0.1))
      .playWithDuration(start, length)

    synthPlayer()
      .triangle(staticControl(spectrumNote), relativePercControl(0.001, velocity / 50, attack(1), Right(Instrument.WELCH)))
      .ring(staticControl(spectrum(note + 2)))
      .lowPass(staticControl(spectrum(note)))
      .pan(staticControl(pan + 0.1))
      .playWithDuration(start, length)

    synthPlayer()
      .pinkNoise(relativePercControl(0.001, velocity, attack(2), Right(Instrument.WELCH)))
      .bandPass(staticControl(spectrumNote), staticControl(spectrumNote / 50000.0))
      .ring(staticControl(spectrum(note + 1)))
      .lowPass(staticControl(spectrum(note + 1)))
      .pan(lineControl(pan - 0.2, pan + 0.2))
      .playWithDuration(start, length)
  }

  var noiseFundamental: Double = Note.noteToHertz("c1")
  var noiseSecond: Double = Note.noteToHertz("fiss1")

  def noiseNote(start: Double = 0, noteSuggestion: Int, spread: Int, length: Double, pan: Double, velocity: Double, attack: Seq[Double], spect: (Double, Double) = (noiseFundamental, noiseSecond)): Unit = {
    val (fund, sec) = spect
    val fact = Spectrum.makeFact(fund, sec)

    val note = math.max(noteSuggestion, spread)
    val spectrum = Spectrum.makeSpectrum2(fund, fact, 50)

    println(s"noise note $note spread $spread pitch ${spectrum(note - spread)} ${spectrum(note)} ${spectrum(note + spread)} length $length pan $pan velocity $velocity attack $attack spect $spect")

    synthPlayer()
      .whiteNoise(relativePercControl(0.001, velocity * 2, attack.head, Right(Instrument.WELCH)))
      .lowPass(relativePercControl(spectrum(note), spectrum(note + spread), attack.head, Right(Instrument.WELCH)))
      .highPass(relativePercControl(spectrum(note), spectrum(note - spread), attack.head, Right(Instrument.WELCH)))
      .pan(lineControl(pan + 0.2, pan - 0.2))
      .playWithDuration(start, length)

    synthPlayer()
      .pinkNoise(relativePercControl(0.001, velocity * 2, attack(1), Right(Instrument.WELCH)))
      .lowPass(relativePercControl(spectrum(note + spread), spectrum(note), attack(1), Right(Instrument.WELCH)))
      .highPass(relativePercControl(spectrum(note - spread), spectrum(note), attack(1), Right(Instrument.WELCH)))
      .pan(lineControl(pan - 0.2, pan + 0.2))
      .playWithDuration(start, length)
  }

  val shortLengths = WeightedRandom(Seq((8, 0.2), (5, 0.3), (3, 0.5)))
  val attacks = Picker(Seq(0.1, 0.3, 0.4, 0.5, 0.6, 0.7))
  val spreads = WeightedRandom(Seq((1, 0.5), (2, 0.3), (3, 0.2)))
  val noisePitch = WeightedRandom(Seq((0, 0.2), (1, 0.8)))

  def noteHandle(key: Int, velocity: Int): Unit = {
    if (client.clockTime <= 0) client.resetClock
    val start = (System.currentTimeMillis() - (client.clockTime + 1900)) / 1000.0
    val baseLen = shortLengths.choose()
    val attack = attacks.pick(4)
    val pan = (random.nextDouble() * 2) - 1

    noisePitch.choose() match {
      case 0 =>
        pitchNote(start, key % 24, math.min((127.0 / velocity) * baseLen, 35), pan, velocity, attack)
      case 1 =>
        noiseNote(start, key % 24, spreads.choose(), math.min((127.0 / velocity) * baseLen, 35), pan, velocity / 127.0, attack)
    }
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
