#!/usr/bin/env fish
# based on https://gist.github.com/sanfilippopablo/3c28f5f3f9d0e9a8a605e4b3b009d6ea
# with perl removed.

function get_git_provider_url --argument filename line_number
    # Check if inside a git repository
    if not git rev-parse --is-inside-work-tree >/dev/null 2>&1
        echo "Error: Not inside a git repository."
        return 1
    end

    # Get the remote URL
    set remote_url (git config --get remote.origin.url)
    if test -z "$remote_url"
        echo "Error: No remote origin found."
        return 1
    end

    # Determine the provider (GitHub, GitLab, etc.)
    if string match -q '*github.com*' $remote_url
        set provider github
    else if string match -q '*gitlab.com*' $remote_url
        set provider gitlab
    else
        echo "Error: Unsupported git provider."
        return 1
    end

    # Extract the repo path and branch
    set repo_url (string replace -r '(https?://)?(git@)?(github|gitlab)\.com[:/]' '' $remote_url)
    set repo_path (string replace -r '\.git$' '' $repo_url)
    set branch (git rev-parse --abbrev-ref HEAD)

    # Generate the URL based on the provider
    switch $provider
        case github
            echo "https://github.com/$repo_path/blob/$branch/$filename#L$line_number"
        case gitlab
            echo "https://gitlab.com/$repo_path/-/blob/$branch/$filename#L$line_number"
    end
end


# Ensure correct number of arguments
if [ (count $argv) -ne 2 ]
    echo "Usage: script.fish <filename> <line_number>"
    return 1
end

get_git_provider_url $argv[1] $argv[2]
