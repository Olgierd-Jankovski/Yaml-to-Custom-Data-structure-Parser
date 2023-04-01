# Yaml-to-Custom-Data-structure-Parser
This is a parser written in Haskell that parses a Yaml string and converts it to a custom data structure similar to JSON. it includes various monadic parsers to handle different types of YAML elements such as empty dictionaries, empty lists, null values, quoted strings, etc.. In a short, the entire directory is a bimaru game, where we had to implement functions to make the client run. There are 3 main tasks distributed in 3 different haskell files


# Installation
  1. Clone this repository **[git clone ttps://github.com/your_username/your_project.git](https://github.com/Olgierd199/Yaml-to-Custom-Data-structure-Parser.git)**
  2. Install Haskell Stack
  3. Run "stack run bimaru-client-3 -- $TOKEN" (the TOKEN is a unique UUID of the game: 9cfda689-fee4-4a87-b82b-49dd379f3cad)
  4. Run "Stack test" to run the test suite
  
# Parsers
When it comes to the program's functionality, there are two parsers: parser that converts YAML string to JSON string and parser that parses JSON string into the Custom Data Structure - Document:
![image](https://user-images.githubusercontent.com/93738688/229284570-ee2749ab-3154-49bb-872f-56b319f43695.png)

The following parsers are available:
  1. **parseYamlEmptyDmap** - Parses an empty dictionary (`{}`)
  2. **parseYamlEmptyDlist** - Parses an empty list (`[]`)
  3. **parseYamlDnull** - Parses a null value (`null`)
  4. **parseYamlDoubleQuotedString** - Parses a double-quoted string.
  5. **parseYamlSinglyQuotedString** - Parses a single-quoted string.
  6. **parseYamlDlist** - Parses a list of elements
  7. **parseYamlDmap** - Parses a dictionary with key-value pairs.
  8. **parseYamlDmapElement** - Parses a single key-value pair within a dictionary.
  9. **parseYamlDmapElement'** Parses the value of a key-value pair within  a dictionary.
  10. **parseYamlDstring** - Parses an unquoted string.
  11. **parseYamlPossibleNegativeInteger** - Parses a negative integer.
  
# Contributing
Contributions are welcome! If you find a bug or want to add a new feature, please open an issue or a pull request.
