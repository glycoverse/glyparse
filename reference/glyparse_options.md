# Native Parsing Options

Native format decoding and normalization use up to two CPU workers for
batches of at least 256 distinct, normalized inputs. Smaller batches are
decoded serially. Set `options(glyparse.threads = 1L)` to disable
parallel decoding, or set an integer between 1 and 64 to choose the
worker limit. Each worker has its own residue cache. R object
allocation, validation, progress reporting, and graph construction run
on the calling thread. No complete input or parsed result is cached
between calls.
