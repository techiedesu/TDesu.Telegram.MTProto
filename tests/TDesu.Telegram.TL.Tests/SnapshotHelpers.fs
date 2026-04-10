namespace TDesu.Telegram.TL.Tests

open System.IO
open NUnit.Framework

[<AutoOpen>]
module SnapshotHelpers =

    let private snapshotDir = Path.Combine(__SOURCE_DIRECTORY__, "Snapshots")
    let private testDataDir = Path.Combine(__SOURCE_DIRECTORY__, "TestData")

    /// Normalize line endings and trim trailing whitespace.
    let private normalize (s: string) = s.Replace("\r\n", "\n").TrimEnd()

    /// Replace non-deterministic timestamps in generated code.
    let normalizeTimestamp (code: string) =
        code.Split('\n')
        |> Array.map (fun line ->
            if line.TrimStart().StartsWith("// Auto-generated at") then
                "// Auto-generated at [TIMESTAMP]"
            else
                line)
        |> String.concat "\n"

    /// Read a file from the TestData directory.
    let readTestData (fileName: string) =
        File.ReadAllText(Path.Combine(testDataDir, fileName))

    /// Assert that actual output matches the golden snapshot file.
    /// First run: creates the snapshot and passes with a message.
    /// Mismatch: writes .actual file and fails with diff.
    let assertMatchesSnapshot (actual: string) (snapshotName: string) =
        if not (Directory.Exists snapshotDir) then
            Directory.CreateDirectory snapshotDir |> ignore

        let expectedPath = Path.Combine(snapshotDir, $"{snapshotName}.expected")
        let actualPath = Path.Combine(snapshotDir, $"{snapshotName}.actual")
        let normalizedActual = normalize actual

        if not (File.Exists expectedPath) then
            File.WriteAllText(expectedPath, normalizedActual)
            Assert.Pass $"Created snapshot: {snapshotName}.expected"
        else
            let expected = normalize (File.ReadAllText expectedPath)

            if normalizedActual <> expected then
                File.WriteAllText(actualPath, normalizedActual)
                let el = expected.Split('\n')
                let al = normalizedActual.Split('\n')
                let maxLen = max el.Length al.Length
                let diffs = ResizeArray<string>()

                for i in 0 .. maxLen - 1 do
                    let e =
                        if i < el.Length then
                            el[i]
                        else
                            "<EOF>"

                    let a =
                        if i < al.Length then
                            al[i]
                        else
                            "<EOF>"

                    if e <> a && diffs.Count < 15 then
                        diffs.Add $"L{i + 1}:\n  - {e}\n  + {a}"

                let diffText = String.concat "\n" diffs
                Assert.Fail $"Snapshot mismatch: {snapshotName}\n\n{diffText}"
            elif File.Exists actualPath then
                File.Delete actualPath

    /// Force-update a snapshot file.
    let updateSnapshot (content: string) (snapshotName: string) =
        if not (Directory.Exists snapshotDir) then
            Directory.CreateDirectory snapshotDir |> ignore

        let expectedPath = Path.Combine(snapshotDir, $"{snapshotName}.expected")
        File.WriteAllText(expectedPath, normalize content)
