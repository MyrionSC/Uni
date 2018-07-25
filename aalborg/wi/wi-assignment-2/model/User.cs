using System;
using System.Collections.Generic;

namespace wi_assignment_2
{
    public class User
    {
        public string Name;
        public HashSet<string> Friends;
        public string Summary;
        public string Review;

        public User ()
        {
            Name = "";
            Friends = new HashSet<string>();
            Summary = "";
            Review = "";
        }

        public User(string name, HashSet<string> friends, string summary, string review)
        {
            Name = name;
            Friends = friends;
            Summary = summary;
            Review = review;
        }
    }
}