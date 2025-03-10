int main () {
  foo();
  snd_assert(!atomic_read(&substream->runtime->mmap_count), );
}
