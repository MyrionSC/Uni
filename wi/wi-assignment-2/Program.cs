using System;
using System.Collections.Generic;
using System.IO;


namespace wi_assignment_2
{
    internal class Program
    {
        public static void Main(string[] args)
        {
            HashSet<User> users = Parser.ParseUsers("friendships.txt");

            Console.WriteLine(users.Count);
            Console.WriteLine();
            foreach (User user in users)
            {
                foreach (string userFriend in user.Friends)
                {
                    Console.WriteLine(userFriend);
                }
                break;
            }
            
            
            
            
        }
    }
    
}