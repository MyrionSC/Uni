using System;
using System.Collections.Generic;
using System.IO;

namespace wi_assignment_2
{
    public class Parser
    {
        public static List<User> ParseUsers(string filepath)
        {
            List<User> users = new List<User>();
            string[] friendship = File.ReadAllLines(filepath);

            User parsingUser = new User();
            foreach (string line in friendship)
            {
                if (line.StartsWith("user:"))
                {
                    parsingUser.Name = line.Substring(6, line.Length - 6);
                }
                else if (line.StartsWith("friends:"))
                {
                    string friends = line.Substring(8, line.Length - 8);
                    string[] friendsList = friends.Trim().Split('\t');
                    parsingUser.Friends = new HashSet<string>(friendsList);
                }
                else if (line.StartsWith("summary:"))
                {
                    parsingUser.Summary = line.Substring(9, line.Length - 9).TrimEnd();
                }
                else if (line.StartsWith("review:"))
                {
                    parsingUser.Review = line.Substring(8, line.Length - 8);
                }
                else
                {
                    users.Add(parsingUser);
                    parsingUser = new User();
                }
            }

            return users;
        }
    }
}