using System.Linq;

public class Number {
    public Number(int num) {
        full = num;
    }

    public int full { get; set; }
    private int hundr { get => full - (full % 100); }
    public int hundreds { get => hundr / 100; }
    private int tn { get => (full - hundr) - ((full - hundr) % 10); }
    public int tens { get => tn / 10; }
    public int ones { get => (full - hundr - tn); }

    public override string ToString() => string.Format("{0}\t{1}\t{2}\t{3}", full, hundreds, tens, ones);
}

var unsortedNumbers = new List<Number>() {
    new Number(400),
    new Number(600),
    new Number(521),
    new Number(500),
    new Number(512),
    new Number(599),
    new Number(510),
    new Number(520),
    new Number(511),
    new Number(529),
};

// Cheater sort
Console.WriteLine("Cheater sort:");
unsortedNumbers.OrderBy(n => n.full).ToList().ForEach(n => Console.WriteLine(n.ToString()));
//Console.WriteLine("Two-level sort (note that this is incomplete sorting for our purposes):");
//unsortedNumbers.OrderBy(n => n.hundreds).ThenBy(n => n.tens).ToList().ForEach(n => Console.WriteLine(n.ToString()));
Console.WriteLine("Mixed-up tertiary sort:");
unsortedNumbers.OrderBy(n => n.hundreds).ThenBy(n => n.ones).ThenBy(n => n.tens).ToList().ForEach(n => Console.WriteLine(n.ToString()));
Console.WriteLine("Tertiary sort:");
unsortedNumbers.OrderBy(n => n.hundreds).ThenBy(n => n.tens).ThenBy(n => n.ones).ToList().ForEach(n => Console.WriteLine(n.ToString()));
