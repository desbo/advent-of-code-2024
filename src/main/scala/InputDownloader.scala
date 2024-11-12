package aoc

import cats.effect.IO
import cats.syntax.all.*
import sttp.client3.{SimpleHttpClient, SttpBackend, UriContext}
import sttp.client3.quick.*
import os.*

trait InputDownloader:
  def download(year: Int, day: Int): IO[String]

object InputDownloader:
  def default(token: String): InputDownloader =
    new Cached(new Sttp(SimpleHttpClient(), token))

  class Sttp(client: SimpleHttpClient, token: String) extends InputDownloader:
    def download(year: Int, day: Int): IO[String] =
      val req = basicRequest
        .get(uri"https://adventofcode.com/$year/day/$day/input")
        .cookie("session", token)

      IO.fromEither:
        client
          .send(req)
          .body
          .leftMap: e =>
            new Exception(e)

  class Cached(underlying: InputDownloader) extends InputDownloader:
    def path(year: Int, day: Int): Path = os.pwd / "input" / year.toString / day.toString

    override def download(year: Int, day: Int): IO[String] =
      val p = path(year, day)
      if os.exists(p) then os.read(p).pure[IO]
      else
        underlying
          .download(year, day)
          .flatTap: data =>
            IO(os.write(p, data, createFolders = true))
