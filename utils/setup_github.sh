#!/bin/bash

# GitHub Repository Setup Script
# Run this script to push your Business Model Simulator to GitHub

echo "🚀 Business Model Simulator - GitHub Setup"
echo "========================================="
echo ""

# Check if git is initialized
if [ ! -d ".git" ]; then
    echo "❌ No git repository found. Please run this script from the project directory."
    exit 1
fi

echo "📝 Please provide your GitHub username:"
read -p "GitHub username: " username

if [ -z "$username" ]; then
    echo "❌ Username cannot be empty"
    exit 1
fi

echo ""
echo "🔗 Setting up GitHub remote..."
git remote remove origin 2>/dev/null || true
git remote add origin "https://github.com/${username}/business-model-simulator.git"

echo "📤 Pushing to GitHub..."
echo "Note: You may be prompted for your GitHub credentials"
echo ""

git push -u origin main

if [ $? -eq 0 ]; then
    echo ""
    echo "✅ Successfully pushed to GitHub!"
    echo "🌐 Your repository is now available at:"
    echo "   https://github.com/${username}/business-model-simulator"
    echo ""
    echo "📋 Next steps:"
    echo "1. Go to your GitHub repository"
    echo "2. Add a description and topics"
    echo "3. Consider adding a screenshot to the README"
    echo "4. Star the repository if you like it! ⭐"
else
    echo ""
    echo "❌ Failed to push to GitHub"
    echo "💡 Make sure you have:"
    echo "   1. Created the repository on GitHub first"
    echo "   2. Correct GitHub credentials"
    echo "   3. Internet connection"
    echo ""
    echo "📖 See GITHUB_SETUP.md for detailed instructions"
fi