export default function indicateLoading(updateLoading, waitMs = 200) {
  let canceled = false
  let finished = false
  let loading = false

  setTimeout(() => {
    if (!finished && !canceled) {
      loading = true
      updateLoading(true)
    }
  }, waitMs)

  return {
    get isLoading() {
      return loading
    },

    finished() {
      finished = true
      if (loading && !canceled) {
        loading = false
        updateLoading(false)
      }
    },

    cancel() {
      canceled = true
    },
  }
}

