<main>
<article class="day-desc"><h2>--- Day 5: Supply Stacks ---</h2><p>The expedition can depart as soon as the final supplies have been unloaded from the ships. Supplies are stored in stacks of marked <em>crates</em>, but because the needed supplies are buried under many other crates, the crates need to be rearranged.</p>
<p>The ship has a <em>giant cargo crane</em> capable of moving crates between stacks. To ensure none of the crates get crushed or fall over, the crane operator will rearrange them in a series of carefully-planned steps. After the crates are rearranged, the desired crates will be at the top of each stack.</p>
<p>The Elves don't want to interrupt the crane operator during this delicate procedure, but they forgot to ask her <em>which</em> crate will end up where, and they want to be ready to unload them as soon as possible so they can embark.</p>
<p>They do, however, have a drawing of the starting stacks of crates <em>and</em> the rearrangement procedure (your puzzle input). For example:</p>
<pre><code>    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2
</code></pre>
<p>In this example, there are three stacks of crates. Stack 1 contains two crates: crate <code>Z</code> is on the bottom, and crate <code>N</code> is on top. Stack 2 contains three crates; from bottom to top, they are crates <code>M</code>, <code>C</code>, and <code>D</code>. Finally, stack 3 contains a single crate, <code>P</code>.</p>
<p>Then, the rearrangement procedure is given. In each step of the procedure, a quantity of crates is moved from one stack to a different stack. In the first step of the above rearrangement procedure, one crate is moved from stack 2 to stack 1, resulting in this configuration:</p>
<pre><code>[D]        
[N] [C]    
[Z] [M] [P]
 1   2   3 
</code></pre>
<p>In the second step, three crates are moved from stack 1 to stack 3. Crates are moved <em>one at a time</em>, so the first crate to be moved (<code>D</code>) ends up below the second and third crates:</p>
<pre><code>        [Z]
        [N]
    [C] [D]
    [M] [P]
 1   2   3
</code></pre>
<p>Then, both crates are moved from stack 2 to stack 1. Again, because crates are moved <em>one at a time</em>, crate <code>C</code> ends up below crate <code>M</code>:</p>
<pre><code>        [Z]
        [N]
[M]     [D]
[C]     [P]
 1   2   3
</code></pre>
<p>Finally, one crate is moved from stack 1 to stack 2:</p>
<pre><code>        [<em>Z</em>]
        [N]
        [D]
[<em>C</em>] [<em>M</em>] [P]
 1   2   3
</code></pre>
<p>The Elves just need to know <em>which crate will end up on top of each stack</em>; in this example, the top crates are <code>C</code> in stack 1, <code>M</code> in stack 2, and <code>Z</code> in stack 3, so you should combine these together and give the Elves the message <code><em>CMZ</em></code>.</p>
<p><em>After the rearrangement procedure completes, what crate ends up on top of each stack?</em></p>
</article>
<p>Your puzzle answer was <code>HNSNMTLHQ</code>.</p><article class="day-desc"><h2 id="part2">--- Part Two ---</h2><p>As you watch the crane operator expertly rearrange the crates, you notice the process isn't following your prediction.</p>
<p>Some mud was covering the writing on the side of the crane, and you quickly wipe it away. The crane isn't a CrateMover 9000 - it's a <em><span title="It's way better than the old CrateMover 1006.">CrateMover 9001</span></em>.</p>
<p>The CrateMover 9001 is notable for many new and exciting features: air conditioning, leather seats, an extra cup holder, and <em>the ability to pick up and move multiple crates at once</em>.</p>
<p>Again considering the example above, the crates begin in the same configuration:</p>
<pre><code>    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 
</code></pre>
<p>Moving a single crate from stack 2 to stack 1 behaves the same as before:</p>
<pre><code>[D]        
[N] [C]    
[Z] [M] [P]
 1   2   3 
</code></pre>
<p>However, the action of moving three crates from stack 1 to stack 3 means that those three moved crates <em>stay in the same order</em>, resulting in this new configuration:</p>
<pre><code>        [D]
        [N]
    [C] [Z]
    [M] [P]
 1   2   3
</code></pre>
<p>Next, as both crates are moved from stack 2 to stack 1, they <em>retain their order</em> as well:</p>
<pre><code>        [D]
        [N]
[C]     [Z]
[M]     [P]
 1   2   3
</code></pre>
<p>Finally, a single crate is still moved from stack 1 to stack 2, but now it's crate <code>C</code> that gets moved:</p>
<pre><code>        [<em>D</em>]
        [N]
        [Z]
[<em>M</em>] [<em>C</em>] [P]
 1   2   3
</code></pre>
<p>In this example, the CrateMover 9001 has put the crates in a totally different order: <code><em>MCD</em></code>.</p>
<p>Before the rearrangement process finishes, update your simulation so that the Elves know where they should stand to be ready to unload the final supplies. <em>After the rearrangement procedure completes, what crate ends up on top of each stack?</em></p>
</article>
<p>Your puzzle answer was <code>RNLFDJMCT</code>.</p><p class="day-success">Both parts of this puzzle are complete! They provide two gold stars: **</p>
<p>At this point, you should <a href="/2022">return to your Advent calendar</a> and try another puzzle.</p>
<p>If you still want to see it, you can <a href="5/input" target="_blank">get your puzzle input</a>.</p>
<p>You can also <span class="share">[Share<span class="share-content">on
  <a href="https://twitter.com/intent/tweet?text=I%27ve+completed+%22Supply+Stacks%22+%2D+Day+5+%2D+Advent+of+Code+2022&amp;url=https%3A%2F%2Fadventofcode%2Ecom%2F2022%2Fday%2F5&amp;related=ericwastl&amp;hashtags=AdventOfCode" target="_blank">Twitter</a>
  <a href="javascript:void(0);" onclick="var mastodon_instance=prompt('Mastodon Instance / Server Name?'); if(typeof mastodon_instance==='string' && mastodon_instance.length){this.href='https://'+mastodon_instance+'/share?text=I%27ve+completed+%22Supply+Stacks%22+%2D+Day+5+%2D+Advent+of+Code+2022+%23AdventOfCode+https%3A%2F%2Fadventofcode%2Ecom%2F2022%2Fday%2F5'}else{return false;}" target="_blank">Mastodon</a
></span>]</span> this puzzle.</p>
</main>
