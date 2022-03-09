function manageKey(key) {
  return (defaultValue) => {
    return {
      get: () => {
        const stringifyValue = localStorage.getItem(key)

        if (!stringifyValue) {
          return defaultValue
        }

        let value
        try {
          value = JSON.parse(localStorage.getItem(key))
        }
        catch (e) {
          console.warn(`Json parsing failed for the retrieving of key ${key}, defaultValue is returned instead.`, e)
          return defaultValue
        }

        if (!value) {
          return defaultValue
        }

        return value
      },

      set: (value) => {
        if (!value) {
          value = defaultValue
        }

        // let stringifyValue

        // try {
        //   stringifyValue = JSON.stringify(value)
        // }
        // catch (e) {
        //   stringifyValue = Json.stringifyValue({})
        //   console.warn(`Json stringify error for the insertion in ${key}, empty table is inserted instead of the original value.`)

        localStorage.setItem(key, value)

        return true
      }
    }
  }
}

export default manageKey
