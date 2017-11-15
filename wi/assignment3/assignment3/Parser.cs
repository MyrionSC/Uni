using System.Collections.Generic;
using System.IO;
using assignment3.model;

namespace assignment3
{
    public class Parser
    {
        public static List<Rating> ParseRatings(string path)
        {
            List<Rating> ratings = new List<Rating>();
            foreach (string line in File.ReadAllLines(path))
            {
                string[] props = line.Split('\t');
                ratings.Add(new Rating(int.Parse(props[0]), int.Parse(props[1]),int.Parse(props[2])));
            }
            return ratings;
        }
    }
}